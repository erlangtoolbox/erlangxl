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
-module(ldb_server).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, put/2, get/1, get_async/1]).

start_link(Location) ->
    error_logger:info_report("leveldb started at " ++ Location),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Location], []).

put(Key, Value) -> gen_server:call(?MODULE, {put, Key, Value}).
get(Key) -> gen_server:call(?MODULE, {get, Key}, infinity).

%% this is an idiocy from ancient times. should be fixed when there is no rush.
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

%% this is an idiocy from ancient times. should be fixed when there is no rush.
handle_cast({get, From, Key}, Db) ->
    spawn_link(fun() ->
        From ! ldbget(Db, Key)
    end),
    {noreply, Db}.

handle_info(_Msg, Db) -> {noreply, Db}.
code_change(_Old, Db, _Extra) -> {ok, Db}.
terminate(Reason, _Db) -> error_logger:error_report({terminated, Reason}), ok.

