%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @copyright (C) 2014, strikead.com
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xl_memdb_rsync).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-include("xl_memdb_memory.hrl").

-compile({parse_transform, do}).

%% API
-export([sync/4, start/3, stop/1]).

-spec(start(atom(), term(), #xl_memdb_memory{}) -> ok).
start(Name, Options, Memory) ->
    xl_scheduler:interval_with_state(Name, xl_lists:kvfind(replication_interval, Options, 5000), ?MODULE, sync, [Name, Options, Memory], 0).


-spec(stop(atom()) -> ok).
stop(Name) ->
    xl_scheduler:cancel(Name).

%% @hidden
sync(Name, Options, Memory, LastRSync) ->
    do([error_m ||
        case xl_lists:kvfind(replication_master_node, Options) of
            {ok, MasterNode} ->
                error_logger:info_msg("~p rsync: start~n", [Name]),
                NewLastRSync = xl_calendar:now_micros(),
                R = do([error_m ||
                    MasterDb <- xl_lists:ekvfind(replication_master_db, Options),
                    Treshold <- return(xl_lists:kvfind(replication_bucket, Options, 20)),
                    SafeInterval <- return(xl_lists:kvfind(replication_safe_interval, Options, 2000000)),
                    S <- xl_rpc:call(MasterNode, xl_memdb, updates, [MasterDb, LastRSync - SafeInterval]),
                    xl_stream:eforeach(fun(Items) ->
                        case lists:partition(fun(X) -> element(1, X) == ok end, Items) of
                            {OkItems, []} ->
                                error_logger:info_msg("~p rsync: ~p items read~n", [Name, length(OkItems)]),
                                xl_memdb_memory:store(Memory, lists:map(fun({ok, X}) -> X end, OkItems));
                            {_, [Error | _]} -> Error
                        end
                    end, xl_stream:listn(Treshold, xl_stream:to_rpc_stream(MasterNode, S)))
                ]),
                error_logger:info_msg("~p rsync: result ~p~n", [Name, R]),
                NewLastRSync;
            undefined -> LastRSync
        end
    ]).
