-module(strikead_httpc).

-include_lib("strikead_stdlib/include/gen_server_specs.hrl").

-behaviour(gen_server).
-compile({parse_transform, do}).

%% -----------------------------------------------------------------------------
%% API Function Exports
%% -----------------------------------------------------------------------------
-export([start_link/2, stop/0, call/1]).

%% -----------------------------------------------------------------------------
%% gen_server Function Exports
%% -----------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	terminate/2, code_change/3]).

%% -----------------------------------------------------------------------------
%% API Function Definitions
%% -----------------------------------------------------------------------------
-spec start_link/2 :: (atom(), atom()) -> {ok, pid()} | ignore | {error, term()}.
start_link(App, Profile) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {App, Profile}, []).

-spec stop/0 :: () -> ok.
stop() -> gen_server:cast(?MODULE, stop).

-spec call/1 :: (string()) -> error_m:monad(tuple()).
call(Url) -> gen_server:call(?MODULE, {simple_request, Url}).

%% -----------------------------------------------------------------------------
%% gen_server Function Definitions
%% -----------------------------------------------------------------------------
-record(state, {request_opts, profile}).

init({App, Profile}) ->
	do([error_m||
		inets:start(httpc, [{profile, Profile}]),
		ClientOpts <- application:get_env(App, Profile),
		httpc:set_options(
			element(2, strikead_lists:keyfind(client, 1,
				ClientOpts, {client, []})),
			Profile),
		return(#state{
			request_opts=element(2,
				strikead_lists:keyfind(request, 1, ClientOpts, {request, []})),
			profile=Profile
		})
	]).

handle_call({simple_request, Url}, _From,
	State=#state{profile=Profile, request_opts=Opts}) ->
	Result = case httpc:request(get, {Url, []}, Opts, [], Profile) of
		{ok, {{_, Code, Reason},_, _}} -> {ok, {Code, Reason}};
		E = {error, _} -> E
	end,
	{reply, Result, State};
handle_call(_Request, _From, State) -> {noreply, ok, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
	inets:stop(httpc, s2s_http),
	ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% -----------------------------------------------------------------------------
%% Internal Function Definitions
%% -----------------------------------------------------------------------------

