%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

-spec delete/2 :: (atom(), string()) -> ok.
delete(Name, Id) -> gen_server:call(Name, {delete, Id}).

-spec get/2 :: (atom(), string()) -> option_m:monad(term()).
get(Name, Id) -> gen_server:call(Name, {get, Id}).

-spec stop/1 :: (atom()) -> ok.
stop(Name) -> gen_server:call(Name, stop).

init({Name, Identify, StoreModule, Options}) ->
    do([error_m ||
        P <- persist:open(Name, Identify, StoreModule, Options),
        return(#state{
            persister = P
        })
    ]).

handle_call(stop, _From, State) ->
    {stop, normal, State};
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

terminate(_Reason, #state{persister = P}) ->
    persist:close(P),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

