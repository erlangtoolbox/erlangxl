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
-module(xl_tdbp).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

%% API
-export([open/4, close/1, store/2, get/2, delete/2, by_index/1, select/1, mapfilter/3, index/1]).
-export_type([identify/0]).

-type(identify() :: fun((term()) -> xl_string:iostring())).

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
        xl_state:set(Name, index, index_build(Options, Objects)),
        xl_state:set(Name, location, Location),
        xl_state:set(Name, identify, Identify),
        xl_state:set(Name, updater_pid, spawn_link(fun update/0)),
        xl_state:set(Name, options, Options)
    ]).


-spec(close(atom()) -> error_m:monad(ok)).
close(Name) ->
    {ok, Pid} = xl_state:value(Name, updater_pid),
    Pid ! stop,
    ok.


-spec(store(atom(), [term()]) -> error_m:monad(ok)).
store(Name, Objects) ->
    mutate(Name, fun() ->
        do([error_m ||
            ETS <- option_m:to_error_m(xl_state:value(Name, ets), no_ets),
            Location <- option_m:to_error_m(xl_state:value(Name, location), no_location),
            Id <- option_m:to_error_m(xl_state:value(Name, identify), no_identify),
            Options <- option_m:to_error_m(xl_state:value(Name, options), no_options),
            xl_lists:eforeach(fun(O) ->
                Wrapped = wrap(O, Id),
                ets:insert(ETS, Wrapped),
                xl_tdb_storage:store(Location, Id(O), Wrapped)
            end, Objects),
            xl_state:set(Name, index, index_build(Options, ets:tab2list(ETS)))
        ])
    end).

-spec(delete(atom(), xl_string:iostring()) -> error_m:monad(ok)).
delete(Name, Id) ->
    mutate(Name, fun() ->
        do([error_m ||
            ETS <- option_m:to_error_m(xl_state:value(Name, ets), no_ets),
            Location <- option_m:to_error_m(xl_state:value(Name, location), no_location),
            Options <- option_m:to_error_m(xl_state:value(Name, options), no_options),
            xl_tdb_storage:delete(Location, Id),
            return(ets:delete(ETS, Id)),
            xl_state:set(Name, index, index_build(Options, ets:tab2list(ETS)))
        ])
    end).

-spec(get(atom(), xl_string:iostring()) -> option_m:monad(term())).
get(Name, Id) ->
    do([option_m ||
        ETS <- xl_state:value(Name, ets),
        case ets:lookup(ETS, Id) of
            [{_, O}] -> {ok, O};
            _ -> undefined
        end
    ]).

-spec(select(atom()) -> [term()]).
select(Name) ->
    {ok, ETS} = xl_state:value(Name, ets),
    unwrap(ets:tab2list(ETS)).

-spec(by_index(pos_integer()) -> fun((term()) -> xl_string:iostring())).
by_index(N) -> fun(X) -> element(N, X) end.

-spec(mapfilter(atom(), xl_lists:kvlist_at(), fun((term(), term()) -> option_m:monad(term()))) -> [term()]).
mapfilter(Name, Q, F) ->
    {ok, Index} = xl_state:value(Name, index),
    {ok, Options} = xl_state:value(Name, options),
    case index_lookup(Q, Options, Index) of
        {ok, Values} -> [V || IV <- Values, Value <- [F(IV)], Value /= undefined, {ok, V} <- [Value]];
        undefined -> []
    end.

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
wrap(Object, Id) -> {Id(Object), Object}.

unwrap(Objects) when is_list(Objects) -> lists:map(fun(O) -> unwrap(O) end, Objects);
unwrap({_, O}) -> O.

index_build(Options, Objects) ->
    case xl_lists:kvfind(index_object, Options) of
        {ok, F} -> xl_uxekdtree:new(lists:foldl(fun(O, Points) -> F(unwrap(O)) ++ Points end, [], Objects), [shared]);
        undefined -> undefined
    end.

index_lookup(_Q, _Options, undefined) -> undefined;
index_lookup(Q, Options, Tree) ->
    case xl_lists:kvfind(index_query, Options) of
        {ok, F} -> xl_uxekdtree:find(F(Q), Tree);
        undefined -> undefined
    end.
