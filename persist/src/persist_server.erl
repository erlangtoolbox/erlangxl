-module(persist_server).

-compile({parse_transform, do}).

-behaviour(gen_server).

%% API
-export([start_link/4, stop/1, select/1, get/2, delete/2, store/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {
    persister :: persist:persister()
}).

start_link(Name, Identify, StoreModule, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Identify, StoreModule, Options}, []).

-spec store/2 :: (atom(), term()) -> ok.
store(Name, X) -> gen_server:call(Name, {store, X}).

-spec select/1 :: (atom()) -> [term()].
select(Name) -> gen_server:call(Name, select).

-spec delete/2 :: (name(), string()) -> ok.
delete(Name, Id) -> gen_server:call(Name, {delete, Id}).

-spec get/2 :: (atom(), string()) -> maybe_m:monad(term()).
get(Name, Id) -> gen_server:call(Name, {get, Id}).

-spec stop/1 :: (atom()) -> maybe_m:monad(ok).
stop(Name) -> gen_server:call(Name, stop).

init({Name, Identify, StoreModule, Options}) ->
    do([error_m ||
        P <- persist:open(Name, Identify, StoreModule, Options),
        return(#state{
            persister = P
        })
    ]).

handle_call(stop, _From, State = #state{persister = P}) ->
    {stop, normal, persist:close(P), State};
handle_call({store, X}, _From, State = #state{persister = P}) ->
    {reply, persist:store(P, X), State};
handle_call({delete, Id}, _From, State = #state{persister = P}) ->
    {reply, persist:delete(P, Id), State};
handle_call({get, Id}, _From, State = #state{persister = P}) ->
    {reply, persist:get(P, Id), State};
handle_call(select, _From, State = #state{persister = P}) ->
    {reply, persist:select(P), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

