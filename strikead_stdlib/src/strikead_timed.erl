-module(strikead_timed).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Time, Module, Fun, Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Time, Module, Fun, Args], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Params = [Time, Module, Fun, Args]) ->
    {ok, TimedTask} = timer:apply_interval(Time, Module, Fun, Args),
    error_logger:info_report({"timed action started", Params}),
    {ok, TimedTask}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, TimedTask) ->
    {ok, cancel} = timer:cancel(TimedTask),
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


% Local Variables:
% indent-tabs-mode: nil
% End:
