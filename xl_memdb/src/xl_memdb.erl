%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @copyright (C) 2013, strikead.com
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xl_memdb).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-compile({parse_transform, do}).

%% API
-export([start/1, store/3, store/2, get/2, stop/1, dump/2, load/2, items/1, status/1, updates/2, start/2, sync/1]).

-spec(start(atom()) -> ok).
start(Name) -> start(Name, []).

-spec(start(atom(), [term()]) -> ok).
start(Name, Options) ->
    do([error_m ||
        xl_state:new(Name),
        xl_state:set(Name, ets, create_ets(Name)),
        xl_state:set(Name, options, Options),
        xl_state:set(Name, replication_last_sync, 0),
        xl_scheduler:interval(xl_string:join_atom([Name, sync]),
            xl_lists:kvfind(replication_interval, Options, 5000), ?MODULE, sync, [Name])
    ]).

-spec(store(atom(), term(), term()) -> ok).
store(Name, Key, Value) ->
    {ok, ETS} = xl_state:value(Name, ets),
    true = ets:insert(ETS, {Key, Value, xl_calendar:now_micros()}),
    ok.

-spec(store(atom(), [{term(), term()}]) -> ok).
store(Name, List) ->
    {ok, ETS} = xl_state:value(Name, ets),
    true = ets:insert(ETS, List),
    ok.

-spec(get(atom(), term()) -> option_m:monad(term())).
get(Name, Key) ->
    do([option_m ||
        ETS <- xl_state:value(Name, ets),
        O <- xl_ets:lookup_object(ETS, Key),
        case O of
            {Key, Value} -> return(Value);   % backward compatibility
            {Key, Value, _Meta} -> return(Value)
        end
    ]).

-spec(stop(atom()) -> ok).
stop(Name) ->
    xl_scheduler:cancel(xl_string:join_atom([Name, sync])),
    case xl_state:value(Name, ets) of
        {ok, ETS} -> ets:delete(ETS);
        undefined -> ok
    end,
    xl_state:delete(Name).

-spec(dump(atom(), file:filename()) -> error_m:monad(ok)).
dump(Name, Location) ->
    do([error_m ||
        ETS <- option_m:to_error_m(xl_state:value(Name, ets), no_ets_in_memory),
        xl_file:ensure_dir(Location),
        ets:tab2file(ETS, Location)
    ]).

-spec(load(atom(), file:filename()) -> error_m:monad(ok)).
load(Name, Location) ->
    {ok, OldETS} = xl_state:value(Name, ets),
    do([error_m ||
        NewETS <- ets:file2tab(Location),
        xl_ets_server:takeover(NewETS),
        xl_state:set(Name, ets, NewETS),
        case OldETS of
            NewETS -> ok;
            _ -> return(ets:delete(OldETS))
        end
    ]).

-spec(items(atom()) -> [{term(), term()}]).
items(Name) ->
    case xl_state:value(Name, ets) of
        {ok, ETS} ->
            lists:map(fun
                (KV = {_, _}) -> KV;  % backward compatibility
                ({Key, Value, _}) -> {Key, Value}
            end, ets:tab2list(ETS));
        _ -> []
    end.

-spec(status(atom()) -> term()).
status(Name) ->
    {ok, ETS} = xl_state:value(Name, ets),
    [{ets, ets:info(ETS)} | xl_state:values(Name)].

-spec(updates(atom(), pos_integer()) -> xl_stream:stream(term())).
updates(Name, Since) ->
    {ok, ETS} = xl_state:evalue(Name, ets),
    xl_ets:cursor(ETS, fun
        ({_Key, _Value}) -> true;   % backward compatibility
        ({_Key, _Value, LastUpdate}) -> LastUpdate > Since
    end).


%% @hidden
create_ets(Name) ->
    xl_ets_server:create(
        xl_string:join_atom([Name, '_', xl_uid:next()]),
        [public, named_table, set]).

%% @hidden
-spec(sync(atom()) -> error_m:monad(ok)).
sync(Name) ->
    do([error_m ||
        Options <- xl_state:evalue(Name, options),
        case xl_lists:kvfind(replication_master_node, Options) of
            {ok, MasterNode} ->
                error_logger:info_msg("~p rsync: start~n", [Name]),
                NewLastRSync = xl_calendar:now_micros(),
                R = do([error_m ||
                    LastRSync <- xl_state:evalue(Name, replication_last_sync),
                    MasterDb <- xl_lists:ekvfind(replication_master_db, Options),
                    Treshold <- return(xl_lists:kvfind(replication_bucket, Options, 20)),
                    SafeInterval <- return(xl_lists:kvfind(replication_safe_interval, Options, 2000000)),
                    S <- xl_rpc:call(MasterNode, xl_memdb, updates, [MasterDb, LastRSync - SafeInterval]),
                    ETS <- xl_state:evalue(Name, ets),
                    xl_stream:eforeach(fun(Items) ->
                        case lists:partition(fun(X) -> element(1, X) == ok end, Items) of
                            {OkItems, []} ->
                                error_logger:info_msg("~p rsync: ~p items read~n", [Name, length(OkItems)]),
                                ets:insert(ETS, lists:map(fun({ok, X}) -> X end, OkItems)),
                                ok;
                            {_, [Error | _]} -> Error
                        end
                    end, xl_stream:listn(Treshold, xl_stream:to_rpc_stream(MasterNode, S))),
                    xl_state:set(Name, replication_last_sync, NewLastRSync)
                ]),
                error_logger:info_msg("~p rsync: result ~p~n", [Name, R]),
                R;
            undefined -> ok
        end
    ]).
