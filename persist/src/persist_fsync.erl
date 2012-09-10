-module(persist_fsync).

-behaviour(xl_gen_periodic).

%% API
-export([start_link/3, stop/1]).

%% xl_gen_periodic callbacks
-export([init/1, handle_action/2, terminate/3]).

% API
-spec start_link/3 :: (ets:tid() | atom(), pos_integer(), module()) -> error_m:monad(pid()).
start_link(ETS, Interval, Storage) ->
    xl_gen_periodic:start_link(?MODULE, {ETS, Storage}, Interval, []).

-spec stop/1 :: (pid()) -> ok.
stop(Pid) -> xl_gen_periodic:stop(Pid).

% xl_gen_periodic callbacks
-record(state, {
    ets = error({undefined, ets}) :: ets:tid() | atom(),
    storage = error({undefined, storage}) :: module()
}).

init({ETS, Storage}) ->
    {ok, #state{
        ets = ETS,
        storage = Storage
    }}.

handle_action(LastAction, State = #state{ets = ETS, storage = Storage}) ->
    fsync(ETS, LastAction, Storage),
    {ok, State}.

terminate(_Reason, LastAction, #state{ets = ETS, storage = Storage}) ->
    fsync(ETS, LastAction, Storage),
    ok.

% Internal functions
fsync(ETS, LastSync, Storage) ->
    Status = xl_lists:eforeach(fun
        ({Id, _, _, true}) -> Storage:delete(Id);
        ({Id, X, _, false}) -> Storage:store(Id, X)
    end, persist:ets_changes(ETS, LastSync)),
    case Status of
        ok -> ok;
        Error -> error_logger:error_msg("cannot fsync: ~p~n", [Error])
    end.
