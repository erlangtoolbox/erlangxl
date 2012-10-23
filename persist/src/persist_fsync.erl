-module(persist_fsync).

-behaviour(xl_gen_periodic).

%% API
-export([start_link/4, stop/1]).

%% xl_gen_periodic callbacks
-export([init/1, handle_action/2, terminate/3]).

% API
-spec start_link/4 :: (ets:tid() | atom(), pos_integer(), boolean(), module()) -> error_m:monad(pid()).
start_link(ETS, Interval, Delete, Storage) ->
    xl_gen_periodic:start_link(?MODULE, {ETS, Delete, Storage}, Interval, []).

-spec stop/1 :: (pid()) -> ok.
stop(Pid) -> xl_gen_periodic:stop(Pid).

% xl_gen_periodic callbacks
-record(state, {
    ets = error({undefined, ets}) :: ets:tid() | atom(),
    delete = true :: boolean(),
    storage = error({undefined, storage}) :: module()
}).

init({ETS, Delete, Storage}) ->
    {ok, #state{
        ets = ETS,
        delete = Delete,
        storage = Storage
    }}.

handle_action(LastAction, State = #state{ets = ETS, delete = Delete, storage = Storage}) ->
    fsync(ETS, LastAction, Delete, Storage),
    {ok, State}.

terminate(_Reason, LastAction, #state{ets = ETS, delete = Delete, storage = Storage}) ->
    fsync(ETS, LastAction, Delete, Storage),
    ok.

% Internal functions
fsync(ETS, LastSync, Delete, Storage) ->
    Status = xl_lists:eforeach(fun
        ({Id, _, _, true}) when Delete -> Storage:delete(Id);
        (R = {Id, _, _, _}) -> Storage:store(Id, R)
    end, persist:ets_changes(ETS, LastSync)),
    case Status of
        ok -> ok;
        Error -> error_logger:error_msg("cannot fsync: ~p~n", [Error])
    end.
