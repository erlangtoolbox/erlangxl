%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(xl_tdb).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

%% API
-export([open/4, close/1, store/2, get/2, delete/2, by_index/1, select/1, nmapfilter/4, index/1, cursor/1, update/3]).
-export_type([identify/0]).

-type(identify() :: fun((term()) -> xl_string:iostring())).

-define(is_deleted(O), element(4, O)).

-spec(open(atom(), file:name(), identify(), xl_lists:kvlist_at()) -> error_m:monad(ok)).
open(Name, Location, Identify, Options) ->
    ETS = ets:new(xl_convert:make_atom([Name, '_tdb']), [
        ordered_set, {keypos, 1}, public
    ]),
    xl_state:new(Name),
    xl_state:set(Name, ets, ETS),
    do([error_m ||
        Objects <- xl_tdb_storage:load(Location),
        lists:foreach(fun(O) -> ets:insert(ETS, O) end, Objects),
        xl_state:set(Name, index, index_build(Options, ETS)),
        xl_state:set(Name, location, Location),
        xl_state:set(Name, identify, Identify),
        xl_state:set(Name, updater_pid, spawn_link(fun update/0)),
        xl_state:set(Name, options, Options),
        xl_state:set(Name, last_fsync, xl_calendar:now_micros()),
        Timer <- timer:apply_interval(xl_lists:kvfind(fsync, Options, 5000), ?MODULE, fsync, [Name]),
        xl_state:set(Name, timer, Timer)
    ]).


-spec(close(atom()) -> error_m:monad(ok)).
close(Name) ->
    {ok, Timer} = xl_state:value(Name, timer),
    timer:cancel(Timer),
    {ok, Pid} = xl_state:value(Name, updater_pid),
    Pid ! stop,
    fsync(Name),
    ok.


-spec(store(atom(), [term()]) -> error_m:monad(ok)).
store(Name, Objects) ->
    mutate(Name, fun() ->
        do([error_m ||
            ETS <- xl_state:evalue(Name, ets),
            Identify <- xl_state:evalue(Name, identify),
            Options <- xl_state:evalue(Name, options),
            lists:foreach(fun(O) -> ets:insert(ETS, wrap(O, Identify)) end, Objects),
            xl_state:set(Name, index, index_build(Options, ETS))
        ])
    end).

-spec(delete(atom(), xl_string:iostring()) -> error_m:monad(ok)).
delete(Name, Id) ->
    mutate(Name, fun() ->
        do([error_m ||
            ETS <- xl_state:evalue(Name, ets),
            Identify <- xl_state:evalue(Name, identify),
            Options <- xl_state:evalue(Name, options),
            case ets_lookup(ETS, Id) of
                {ok, {_Id, O, _LastUpdate, _Deleted}} ->
                    ets:insert(ETS, wrap(O, Identify, true)),
                    xl_state:set(Name, index, index_build(Options, ETS));
                _ -> ok
            end
        ])
    end).

-spec(update(atom(), xl_string:iostring(), fun((term()) -> term())) -> error_m:monad(term())).
update(Name, Id, F) ->
    mutate(Name, fun() ->
        do([option_m ||
            ETS <- xl_state:evalue(Name, ets),
            Identify <- xl_state:evalue(Name, identify),
            Options <- xl_state:evalue(Name, options),
            case ets_lookup(ETS, Id) of
                {ok, {_Id, O, _LastUpdate, _Deleted}} ->
                    R = F(O),
                    ets:insert(ETS, wrap(R, Identify)),
                    xl_state:set(Name, index, index_build(Options, ETS)),
                    return(R);
                _ -> return(undefined)
            end
        ])
    end).

-spec(get(atom(), xl_string:iostring()) -> option_m:monad(term())).
get(Name, Id) ->
    do([option_m ||
        ETS <- xl_state:value(Name, ets),
        case ets_lookup(ETS, Id) of
            {ok, O} when not ?is_deleted(O) -> return(unwrap(O));
            _ -> undefined
        end
    ]).

-spec(select(atom()) -> [term()]).
select(Name) ->
    {ok, ETS} = xl_state:value(Name, ets),
    unwrap(ets:tab2list(ETS)).

-spec(by_index(pos_integer()) -> fun((term()) -> xl_string:iostring())).
by_index(N) -> fun(X) -> element(N, X) end.

-spec(nmapfilter(atom(), non_neg_integer(), xl_lists:kvlist_at(), xl_lists:mapping_predicate(term(), term())) -> [term()]).
nmapfilter(Name, N, Q, F) ->
    {ok, Index} = xl_state:value(Name, index),
    {ok, Options} = xl_state:value(Name, options),
    Random = xl_lists:kvfind(random, Options, false),
    case index_lookup(Q, Options, Index) of
        {ok, Values} when Random -> xl_lists:nshufflemapfilter(N, F, Values);
        {ok, Values} -> xl_lists:nmapfilter(N, F, Values);
        undefined -> []
    end.

-spec(cursor(atom()) -> xl_stream:stream()).
cursor(Name) ->
    {ok, ETS} = xl_state:value(Name, ets),
    xl_stream:mapfilter(
        fun({_Id, O, _LastUpdate, false}) -> {ok, O}; (_) -> undefined end,
        xl_stream:stream(ets:first(ETS),
            fun
                ('$end_of_table') -> empty;
                (Key) ->
                    case ets_lookup(ETS, Key) of
                        {ok, O} -> {O, ets:next(ETS, Key)};
                        _ -> empty
                    end
            end)
    ).

-spec(index(atom()) -> option_m:monad(xl_uxekdtree:tree())).
index(Name) -> xl_state:value(Name, index).

update() ->
    receive
        stop -> ok;
        {mutate, Fun, CallingProcess} ->
            CallingProcess ! Fun(),
            update()
    end.

mutate(Name, Fun) ->
    {ok, Pid} = xl_state:value(Name, updater_pid),
    Pid ! {mutate, Fun, self()},
    receive
        R -> R
    end.

wrap(Objects, Id) when is_list(Objects) -> lists:map(fun(O) -> wrap(O, Id) end, Objects);
wrap(Object, Id) -> wrap(Object, Id, false).

wrap(Object, Id, Deleted) -> {Id(Object), Object, xl_calendar:now_micros(), Deleted}.

unwrap(Objects) when is_list(Objects) -> lists:map(fun(O) -> unwrap(O) end, Objects);
unwrap({_Id, O, _LastUpdate, _Deleted}) -> O.

index_build(Options, ETS) ->
    case xl_lists:kvfind(index_object, Options) of
        {ok, F} -> xl_uxekdtree:new(ets:foldl(fun
            (O, Points) when not ?is_deleted(O) -> F(unwrap(O)) ++ Points;
            (_O, Points) -> Points
        end, [], ETS), [shared]);
        undefined -> undefined
    end.

index_lookup(_Q, _Options, undefined) -> undefined;
index_lookup(Q, Options, Tree) ->
    case xl_lists:kvfind(index_query, Options) of
        {ok, F} -> xl_uxekdtree:find(F(Q), Tree);
        undefined -> undefined
    end.

ets_lookup(ETS, Key) ->
    case ets:lookup(ETS, Key) of
        [O] -> {ok, O};
        _ -> undefined
    end.

fsync(Name) ->
    do([error_m ||
        ETS <- xl_state:evalue(Name, ets),
        Location <- xl_state:evalue(Name, location),
        LastFSync <- xl_state:evalue(Name, last_fsync),
        xl_lists:eforeach(fun
            (Wrapped = {Id, _O, _LastUpdate, false}) -> xl_tdb_storage:store(Location, Id, Wrapped);
            ({Id, _O, _LastUpdate, true}) ->
                do([error_m ||
                    xl_tdb_storage:delete(Location, Id),
                    return(ets:delete(ETS, Id))
                ])
        end, ets:select(ETS, [{{'$1', '$2', '$3', '$4'}, [{'=<', LastFSync, '$3'}], ['$_']}]))
    ]).
