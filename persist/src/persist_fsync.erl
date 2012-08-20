-module(persist_fsync).

-compile({parse_transform, do}).

-behaviour(gen_server).

%% API
-export([start_link/3, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

-record(state, {
    ets :: ets:tid() | atom(),
    timer :: timer:tref(),
    last_sync = strikead_calendar:now_millis() :: integer(),
    storage :: module()
}).

start_link(ETS, Interval, Storage) ->
    gen_server:start_link(?MODULE, {ETS, Interval, Storage}, []).

stop(Pid) -> gen_server:call(Pid, stop).

init({ETS, Interval, Storage}) ->
    do([error_m ||
        Timer <- timer:send_interval(Interval, fsync),
        return(#state{
            ets = ETS,
            timer = Timer,
            storage = Storage
        })
    ]).

handle_call(stop, _From, State = #state{timer = Timer, ets = ETS, last_sync = LastSync, storage = Storage}) ->
    timer:cancel(Timer),
    fsync(ETS, LastSync, Storage),
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(fsync, State=#state{ets = ETS, last_sync = LastSync, storage = Storage}) ->
    {noreply, State#state{
        last_sync = fsync(ETS, LastSync, Storage)
    }};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

fsync(ETS, LastSync, Storage) ->
    List = ets:select(ETS, [{
        {'$1', '$2', '$3', '$4'},
        [{'=<', LastSync, '$3'}],
        ['$_']
    }]),
    Status = strikead_lists:eforeach(fun
        ({Id, _, _, true}) -> Storage:delete(Id);
        ({Id, X, _, _}) -> Storage:store(Id, X)
    end, List),
    case Status of
        ok -> strikead_calendar:now_millis();
        Error ->
            error_logger:error_msg("cannot fsync: ~p~n", [Error]),
            LastSync
    end.