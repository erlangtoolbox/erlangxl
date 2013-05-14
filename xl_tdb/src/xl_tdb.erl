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
-export([open/4, close/1, store/2, get/2, delete/2, by_index/1, select/1, mapfilter/3]).
-export_type([identify/0]).

-type(identify() :: fun((term()) -> xl_string:iostring())).
-type(tdbref() :: pid() | atom()).

-record(xl_tdb_state, {
    location :: file:name(),
    identify :: identify(),
    updater_pid :: pid(),
    options :: xl_lists:kvlist_at(),
    ets :: ets:tid()
}).

-spec(open(atom(), file:name(), identify(), xl_lists:kvlist_at()) -> error_m:monad(pid())).
open(Name, Location, Identify, Options) ->
    ETS = ets:new(xl_convert:make_atom([Name, '_tdb']), [
        ordered_set, {keypos, 1}, public
    ]),
    do([error_m ||
        Objects <- xl_tdb_storage:load(Location),
        Index <- return(index_build(Options, Objects)),
        lists:foreach(fun(O) -> ets:insert(ETS, O) end, Objects),
        TdbState <- return(#xl_tdb_state{
            location = Location,
            identify = Identify,
            updater_pid = spawn_link(fun update/0),
            options = Options,
            ets = ETS
        }),
        Pid <- return(spawn_link(fun() -> loop(Index, TdbState) end)),
        xl_lang:register(Name, Pid)
    ]).


-spec(close(tdbref()) -> error_m:monad(ok)).
close(Ref) ->
    Ref ! stop,
    case is_atom(Ref) of
        true -> xl_lang:unregister(Ref);
        false -> ok
    end.

-spec(store(tdbref(), [term()]) -> error_m:monad(ok)).
store(Ref, Objects) ->
    call(mutate, Ref, fun(#xl_tdb_state{location = Location, identify = Id, options = Options, ets = ETS}) ->
        do([error_m ||
            xl_lists:eforeach(fun(O) ->
                Wrapped = wrap(O, Id),
                ets:insert(ETS, Wrapped),
                xl_tdb_storage:store(Location, Id(O), Wrapped)
            end, Objects),
            return(index_build(Options, ets:tab2list(ETS)))
        ])
    end).

-spec(delete(tdbref(), xl_string:iostring()) -> error_m:monad(ok)).
delete(Ref, Id) ->
    call(mutate, Ref, fun(#xl_tdb_state{location = Location, options = Options, ets = ETS}) ->
        do([error_m ||
            xl_tdb_storage:delete(Location, Id),
            return(ets:delete(ETS, Id)),
            return(index_build(Options, ets:tab2list(ETS)))
        ])
    end).

-spec(get(tdbref(), xl_string:iostring()) -> option_m:monad(term())).
get(Ref, Id) ->
    call(read, Ref, fun(#xl_tdb_state{ets = ETS}) ->
        case ets:lookup(ETS, Id) of
            [{_, O}] -> {ok, O};
            _ -> undefined
        end
    end).

-spec(select(tdbref()) -> [term()]).
select(Ref) -> call(read, Ref, fun(#xl_tdb_state{ets = ETS}) -> unwrap(ets:tab2list(ETS)) end).

-spec(by_index(pos_integer()) -> fun((term()) -> xl_string:iostring())).
by_index(N) -> fun(X) -> element(N, X) end.

-spec(mapfilter(tdbref(), xl_lists:kvlist_at(), fun((term(), tuple()) -> option_m:monad(term()))) -> [term()]).
mapfilter(Ref, Q, F) ->
    call(read_index, Ref, fun(Index, #xl_tdb_state{options = Options}) ->
        case index_lookup(Q, Options, Index) of
            {ok, Values} ->
                xl_lists:mapfilter(fun(X) -> X end, [F(IV) || IV <- Values]) ;
            undefined -> []
        end
    end).

%% Internal functions
loop(Index, Config = #xl_tdb_state{updater_pid = Updater, ets = ETS, options = Options}) ->
    SerialIndex = xl_lists:kvfind(serial_read_index, Options, false),
    SerialRead = xl_lists:kvfind(serial_read, Options, false),
    receive
        {mutate, Fun, CallingProcess} ->
            Updater ! {update, Fun, CallingProcess, self(), Config},
            loop(Index, Config);
        {updated, NewIndex} ->
            loop(NewIndex, Config);
        {read, Fun, CallingProcess} when SerialRead ->
            CallingProcess ! Fun(Config),
            loop(Index, Config);
        {read, Fun, CallingProcess} ->
            spawn(fun() ->
                CallingProcess ! Fun(Config)
            end),
            loop(Index, Config);
        {read_index, Fun, CallingProcess} when SerialIndex ->
            CallingProcess ! Fun(Index, Config),
            loop(Index, Config);
        {read_index, Fun, CallingProcess} ->
            spawn(fun() ->
                CallingProcess ! Fun(Index, Config)
            end),
            loop(Index, Config);
        stop ->
            Updater ! stop,
            ets:delete(ETS),
            ok
    end.

update() ->
    receive
        stop -> ok;
        {update, Fun, CallingProcess, MasterLoop, Config} ->
            case Fun(Config) of
                {ok, UpdateResult} ->
                    MasterLoop ! {updated, UpdateResult},
                    CallingProcess ! ok;
                E -> CallingProcess ! E
            end,
            update()
    end.

call(Msg, Pid, Fun) ->
    Pid ! {Msg, Fun, self()},
    receive
        R -> R
    end.

wrap(Objects, Id) when is_list(Objects) -> lists:map(fun(O) -> wrap(O, Id) end, Objects);
wrap(Object, Id) -> {Id(Object), Object}.

unwrap(Objects) when is_list(Objects) -> lists:map(fun(O) -> unwrap(O) end, Objects);
unwrap({_, O}) -> O.

index_build(Options, Objects) ->
    case xl_lists:kvfind(index_object, Options) of
        {ok, F} ->
            xl_uxekdtree:new(lists:foldl(fun(O, Points) -> F(unwrap(O)) ++ Points end, [], Objects), [xl_lists:kvfind(index_location, Options, shared)]);
        undefined -> undefined
    end.

index_lookup(_Q, _Options, undefined) -> undefined;
index_lookup(Q, Options, Tree) ->
    case xl_lists:kvfind(index_query, Options) of
        {ok, F} -> xl_uxekdtree:find(F(Q), Tree);
        undefined -> undefined
    end.
