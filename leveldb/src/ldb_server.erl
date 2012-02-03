-module(ldb_server).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, put/2, get/1, get_async/1]).

start_link(Location) ->
    error_logger:info_report("leveldb started at " ++ Location),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Location], []).

put(Key, Value) -> gen_server:call(?MODULE, {put, Key, Value}).
get(Key) -> gen_server:call(?MODULE, {get, Key}, infinity).
get_async(Key) ->
    gen_server:cast(?MODULE, {get, self(), Key}),
    receive
        X -> X
    end.

ldbget(Db, Key) ->
    case erleveldb:get(Db, term_to_binary(Key)) of
        {ok, Value} -> binary_to_term(Value);
        _ -> not_found
    end.

init([Location]) ->
    {ok, Db} = erleveldb:open_db(Location, [create_if_missing]),
    {ok, Db}.

handle_call({put, Key, Value}, _From, Db) ->
    erleveldb:put(Db, term_to_binary(Key), term_to_binary(Value)),
    {reply, ok, Db};
handle_call({get, Key}, _From, Db) -> {reply, ldbget(Db, Key), Db}.

handle_cast({get, From, Key}, Db) ->
    spawn_link(fun() ->
        From ! ldbget(Db, Key)
    end),
    {noreply, Db}.

handle_info(_Msg, Db) -> {noreply, Db}.
code_change(_Old, Db, _Extra) -> {ok, Db}.
terminate(Reason, _Db) -> error_logger:error_report({terminated, Reason}), ok.


