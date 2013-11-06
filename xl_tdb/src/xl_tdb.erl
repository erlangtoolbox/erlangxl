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
-export([start_link/4, close/1, store/2, get/2, delete/2, by_index/1, select/1, nmapfilter/4, index/1, cursor/1, update/3, updates/2, delete_all/1, sync/1]).
-export_type([identify/0]).

-type(identify() :: fun((term()) -> xl_string:iostring())).

-define(is_deleted(O), element(4, O)).

-spec(start_link(atom(), file:name(), identify(), xl_lists:kvlist_at()) -> error_m:monad(ok)).
start_link(Name, Location, Identify, Options) ->
    ETS = ets:new(xl_convert:make_atom([Name, '_tdb']), [
        ordered_set, {keypos, 1}, public
    ]),
    xl_state:new(Name),
    xl_state:set(Name, ets, ETS),
    do([error_m ||
        Objects <- xl_tdb_storage:load(
            Location,
            xl_lists:kvfind(version, Options, 1),
            xl_lists:kvfind(migrations, Options, [])
        ),
        lists:foreach(fun(O) -> ets:insert(ETS, O) end, Objects),
        xl_state:set(Name, index, build_index(Options, ETS)),
        xl_state:set(Name, location, Location),
        xl_state:set(Name, identify, Identify),
        Pid <- return(spawn_link(fun() ->
            process_flag(trap_exit, true),
            update(Name)
        end)),
        xl_state:set(Name, updater_pid, Pid),
        xl_state:set(Name, options, Options),
        xl_state:set(Name, last_fsync, xl_calendar:now_micros()),
        xl_state:set(Name, last_rsync, 0),
        xl_scheduler:interval(xl_convert:make_atom([Name, sync]),
            xl_lists:kvfind(sync, Options, 5000), ?MODULE, sync, [Name]),
        return(Pid)
    ]).


-spec(close(atom()) -> error_m:monad(ok)).
close(Name) ->
    case xl_state:evalue(Name, updater_pid) of
        {ok, Pid} ->
            exit(Pid, normal),
            receive
                X -> X
            end;
        E -> E
    end.

-spec(store(atom(), [term()]) -> error_m:monad(ok)).
store(Name, Objects) ->
    mutate(Name, fun() ->
        do([error_m ||
            ETS <- xl_state:evalue(Name, ets),
            Identify <- xl_state:evalue(Name, identify),
            Options <- xl_state:evalue(Name, options),
            lists:foreach(fun(O) -> ets:insert(ETS, wrap(O, Identify)) end, Objects),
            xl_state:set(Name, index, build_index(Options, ETS))
        ])
    end).

-spec(delete(atom(), xl_string:iostring()) -> error_m:monad(ok)).
delete(Name, Id) ->
    mutate(Name, fun() ->
        do([error_m ||
            ETS <- xl_state:evalue(Name, ets),
            Identify <- xl_state:evalue(Name, identify),
            Options <- xl_state:evalue(Name, options),
            case xl_ets:lookup_object(ETS, Id) of
                {ok, {_Id, O, _LastUpdate, _Deleted}} ->
                    ets:insert(ETS, wrap(O, Identify, true)),
                    xl_state:set(Name, index, build_index(Options, ETS));
                _ -> ok
            end
        ])
    end).

-spec(delete_all(atom()) -> error_m:monad(ok)).
delete_all(Name) ->
    mutate(Name, fun() ->
        do([error_m ||
            ETS <- xl_state:evalue(Name, ets),
            Identify <- xl_state:evalue(Name, identify),
            Options <- xl_state:evalue(Name, options),
            xl_ets:foreach(fun
                (_Key, [{_Id, _O, _LastUpdate, true}]) -> ok;
                (_Key, [{_Id, O, _LastUpdate, false}]) ->
                    ets:insert(ETS, wrap(O, Identify, true))
            end, ETS),
            xl_state:set(Name, index, build_index(Options, ETS))
        ])
    end).

-spec(update(atom(), xl_string:iostring(), fun((term()) -> option_m:monad(term()))) -> error_m:monad(term())).
update(Name, Id, F) ->
    mutate(Name, fun() ->
        do([option_m ||
            ETS <- xl_state:evalue(Name, ets),
            Identify <- xl_state:evalue(Name, identify),
            Options <- xl_state:evalue(Name, options),
            Obj <- return(case xl_ets:lookup_object(ETS, Id) of
                {ok, {_Id, O, _LastUpdate, _Deleted}} -> O;
                _ -> undefined
            end),
            case F(Obj) of
                {ok, {context, R, Ctx}} ->
                    ets:insert(ETS, wrap(R, Identify)),
                    xl_state:set(Name, index, build_index(Options, ETS)),
                    return({R, Ctx});
                {ok, R} ->
                    ets:insert(ETS, wrap(R, Identify)),
                    xl_state:set(Name, index, build_index(Options, ETS)),
                    return(R);
                undefined ->
                    return(undefined)
            end
        ])
    end).

-spec(get(atom(), xl_string:iostring()) -> option_m:monad(term())).
get(Name, Id) ->
    do([option_m ||
        ETS <- xl_state:value(Name, ets),
        case xl_ets:lookup_object(ETS, Id) of
            {ok, O} when not ?is_deleted(O) -> {ok, unwrap(O)};
            _ -> undefined
        end
    ]).

-spec(select(atom()) -> [term()]).
select(Name) ->
    {ok, ETS} = xl_state:value(Name, ets),
    xl_lists:mapfilter(fun
        (O) when not ?is_deleted(O) -> {ok, unwrap(O)};
        (_) -> undefined
    end, ets:tab2list(ETS)).

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
    xl_stream:mapfilter(fun({_Id, O, _LastUpdate, false}) -> {ok, O}; (_) -> undefined end, xl_ets:cursor(ETS)).

-spec(index(atom()) -> option_m:monad(xl_uxekdtree:tree())).
index(Name) -> xl_state:value(Name, index).

-spec(sync(atom()) -> error_m:monad(ok)).
sync(Name) ->
    rsync(Name),
    fsync(Name).

-spec(fsync(atom()) -> error_m:monad(ok)).
fsync(Name) ->
    NewLastFSync = xl_calendar:now_micros(),
    do([error_m ||
        Location <- xl_state:evalue(Name, location),
        LastFSync <- xl_state:evalue(Name, last_fsync),
        xl_stream:eforeach(fun(Wrapped = {Id, _O, _LastUpdate, _Deleted}) ->
            xl_tdb_storage:store(Location, Id, Wrapped)
        end, updates(Name, LastFSync)),
        xl_state:set(Name, last_fsync, NewLastFSync)
    ]).

-spec(rsync(atom()) -> error_m:monad(ok)).
rsync(Name) ->
    do([error_m ||
        Options <- xl_state:evalue(Name, options),
        case xl_lists:kvfind(rsync_master_node, Options) of
            {ok, MasterNode} ->
                error_logger:info_msg("~p rsync: start~n", [Name]),
                NewLastRSync = xl_calendar:now_micros(),
                R = do([error_m ||
                    LastRSync <- xl_state:evalue(Name, last_rsync),
                    MasterDb <- xl_lists:ekvfind(rsync_master_db, Options),
                    Treshold <- return(xl_lists:kvfind(rsync_treshold, Options, 20)),
                    SafeInterval <- return(xl_lists:kvfind(rsync_safe_interval, Options, 2000000)),
                    Identify <- xl_state:evalue(Name, identify),
                    S <- xl_rpc:call(MasterNode, xl_tdb, updates, [MasterDb, LastRSync - SafeInterval]),
                    xl_stream:eforeach(fun(Items) ->
                        case lists:partition(fun(X) -> element(1, X) == ok end, Items) of
                            {Ok, []} ->
                                error_logger:info_msg("~p rsync: ~p items read~n", [Name, length(Ok)]),
                                mutate(Name, fun() ->
                                    do([error_m ||
                                        ETS <- xl_state:evalue(Name, ets),
                                        error_logger:info_msg("~p rsync: adding to ets~n", [Name]),
                                        lists:foreach(fun({ok, {_Id, O, _LastMidified, Deleted}}) ->
                                            ets:insert(ETS, wrap(O, Identify, Deleted))
                                        end, Ok),
                                        error_logger:info_msg("~p rsync: building index~n", [Name]),
                                        xl_state:set(Name, index, build_index(Options, ETS)),
                                        error_logger:info_msg("~p rsync: index built~n", [Name])
                                    ])
                                end);
                            {_, [Error | _]} -> Error
                        end
                    end, xl_stream:listn(Treshold, xl_stream:to_rpc_stream(MasterNode, S))),
                    xl_state:set(Name, last_rsync, NewLastRSync)
                ]),
                error_logger:info_msg("~p rsync: result ~p~n", [Name, R]),
                R;
            undefined -> ok
        end
    ]).

-spec(updates(atom(), pos_integer()) -> [term()]).
updates(Name, Since) ->
    {ok, ETS} = xl_state:evalue(Name, ets),
    xl_ets:cursor(ETS, fun({_Id, _O, LastUpdate, _Deleted}) -> LastUpdate > Since end).

update(Name) ->
    receive
        {'EXIT', From, _Reason} ->
            From ! do([error_m ||
                xl_scheduler:cancel(xl_convert:make_atom([Name, sync])),
                fsync(Name),
                xl_state:delete(Name)
            ]);
        {mutate, From, Fun} ->
            From ! Fun(),
            update(Name)
    end.

mutate(Name, Fun) ->
    {ok, Pid} = xl_state:value(Name, updater_pid),
    Pid ! {mutate, self(), Fun},
    receive
        R -> R
    end.

wrap(Objects, Id) when is_list(Objects) -> lists:map(fun(O) -> wrap(O, Id) end, Objects);
wrap(Object, Id) -> wrap(Object, Id, false).

wrap(Object, Id, Deleted) -> {Id(Object), Object, xl_calendar:now_micros(), Deleted}.

unwrap(Objects) when is_list(Objects) -> lists:map(fun(O) -> unwrap(O) end, Objects);
unwrap({_Id, O, _LastUpdate, _Deleted}) -> O.

build_index(Options, ETS) ->
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
