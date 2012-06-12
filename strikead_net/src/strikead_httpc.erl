-module(strikead_httpc).

-include_lib("strikead_stdlib/include/gen_server_specs.hrl").
-include("strikead_httpc.hrl").

-behaviour(gen_server).
-compile({parse_transform, do}).

%% -----------------------------------------------------------------------------
%% API Function Exports
%% -----------------------------------------------------------------------------
-export([start_link/2, stop/1, call/2, post/4]).

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
	gen_server:start_link({local, Profile}, ?MODULE, {App, Profile}, []).

-spec stop/1 :: (atom()) -> ok.
stop(Profile) -> gen_server:cast(Profile, stop).

-spec post/4 :: (atom(), string(), string(), string() | binary()) ->
	error_m:monad({integer(), string(), string()}).
post(Profile, Url, ContentType, Body) ->
	gen_server:call(Profile, {post, Url, ContentType, Body}).

-spec call/2 :: (atom(), string()) -> error_m:monad(tuple()).
call(Profile, Url) -> gen_server:call(Profile, {call, Url}).

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

handle_call({call, Url}, _From,
	State=#state{profile=Profile, request_opts=Opts}) ->
	Result = case httpc:request(get, {Url, []}, Opts, [], Profile) of
		{ok, {{_, Code, Reason}, _, _}} ->
			{ok, #http_response{code=Code, reason=Reason}};
		E = {error, _} -> E
	end,
	{reply, Result, State};
handle_call({post, Url, ContentType, RequestBody}, _From,
	State=#state{profile=Profile, request_opts=Opts}) ->
	Result = case httpc:request(post, {Url, [], ContentType, RequestBody}, Opts, [], Profile) of
		{ok, {{_, Code, Reason}, Headers, Body}} ->
			{ok, #http_response{
				code = Code,
				reason = Reason,
				content_type = strikead_lists:kvfind("content-type", Headers, undefined),
				content = Body
			}};
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

