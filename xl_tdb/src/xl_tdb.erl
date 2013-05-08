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
    objects :: [term()],
    updater_pid :: pid(),
    index :: xl_uxekdtree:tree(),
    options :: xl_lists:kvlist_at()
}).

-spec(open(atom(), file:name(), identify(), xl_lists:kvlist_at()) -> error_m:monad(pid())).
open(Name, Location, Identify, Options) ->
    do([error_m ||
        Objects <- xl_tdb_storage:load(Location),
        Db <- return(#xl_tdb_state{
            location = Location,
            identify = Identify,
            objects = Objects,
            updater_pid = spawn_link(fun update/0),
            index = index_build(Options, Objects),
            options = Options
        }),
        Pid <- return(spawn_link(fun() -> loop(Db) end)),
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
    mutate(Ref, fun(State = #xl_tdb_state{location = Location, objects = StateObjects, identify = Id, options = Options}) ->
        do([error_m ||
            xl_lists:eforeach(fun(O) -> xl_tdb_storage:store(Location, Id(O), wrap(O, Id)) end, Objects),
            NewObjects <- return(wrap(Objects, Id) ++
                lists:foldl(fun(O, FoldObjects) ->
                    lists:keydelete(Id(O), 1, FoldObjects)
                end, StateObjects, Objects)),
            return(State#xl_tdb_state{
                    objects = NewObjects,
                    index = index_build(Options, NewObjects)
            })
        ])

    end).

-spec(delete(tdbref(), xl_string:iostring()) -> error_m:monad(ok)).
delete(Ref, Id) ->
    mutate(Ref, fun(State = #xl_tdb_state{location = Location, objects = StateObjects, options = Options}) ->
        do([error_m ||
            xl_tdb_storage:delete(Location, Id),
            NewObjects <- return(lists:keydelete(Id, 1, StateObjects)),
            return(State#xl_tdb_state{
                    objects = NewObjects,
                    index = index_build(Options, NewObjects)
            })
        ])
    end).

-spec(get(tdbref(), xl_string:iostring()) -> option_m:monad(term())).
get(Ref, Id) ->
    read(Ref, fun(#xl_tdb_state{objects = Objects}) ->
        case xl_lists:keyfind(Id, 1, Objects) of
            {ok, {_, O}} -> {ok, O};
            undefined -> undefined
        end
    end).

-spec(select(tdbref()) -> [term()]).
select(Ref) -> read(Ref, fun(#xl_tdb_state{objects = Objects}) -> unwrap(Objects) end).

-spec(by_index(pos_integer()) -> fun((term()) -> xl_string:iostring())).
by_index(N) -> fun(X) -> element(N, X) end.

-spec(mapfilter(tdbref(), xl_lists:kvlist_at(), fun((term(), tuple()) -> option_m:monad(term()))) -> [term()]).
mapfilter(Ref, Q, F) ->
    read(Ref, fun(#xl_tdb_state{index = Index, options = Options}) ->
        case index_lookup(Q, Options, Index) of
            {ok, Values} ->
                xl_lists:mapfilter(fun(X) -> X end, [F(IV) || IV <- Values]) ;
            undefined -> []
        end
    end).

%% Internal functions
loop(State = #xl_tdb_state{updater_pid = Updater}) ->
    receive
        {mutate, Fun, CallingProcess} ->
            Updater ! {update, Fun, CallingProcess, self(), State},
            loop(State);
        {updated, NewState} ->
            loop(NewState);
        {read, Fun, CallingProcess} ->
            spawn(fun() ->
                CallingProcess ! Fun(State)
            end),
            loop(State);
        stop ->
            Updater ! stop,
            ok
    end.

update() ->
    receive
        stop -> ok;
        {update, Fun, CallingProcess, MasterLoop, State} ->
            case Fun(State) of
                {ok, NewState} ->
                    MasterLoop ! {updated, NewState},
                    CallingProcess ! ok;
                E -> CallingProcess ! E
            end,
            update()
    end.

mutate(Pid, Fun) ->
    Pid ! {mutate, Fun, self()},
    receive
        R -> R
    end.

read(Pid, Fun) ->
    Pid ! {read, Fun, self()},
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
            xl_uxekdtree:new(lists:foldl(fun(O, Points) -> F(unwrap(O)) ++ Points end, [], Objects));
        undefined -> undefined
    end.

index_lookup(_Q, _Options, undefined) -> undefined;
index_lookup(Q, Options, Tree) ->
    case xl_lists:kvfind(index_query, Options) of
        {ok, F} -> xl_uxekdtree:find(F(Q), Tree);
        undefined -> undefined
    end.
