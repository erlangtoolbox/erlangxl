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
-module(xl_state).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([new/1, new/2, get/2, set/3, keys/1, start_link/0, value/2, delete/1, evalue/2, increment/2, increment/3, remove/2]).

%% @todo make it supervised or bind this table to the application master
start_link() ->
    Pid = spawn_link(fun loop/0),
    register(?MODULE, Pid),
    ok.

-spec(new(atom()) -> atom()).
new(Name) -> new(Name, []).

-spec(new(atom(), [term()]) -> error_m:monad(ok)).
new(Name, Options) ->
    ?MODULE ! {init, Name, Options, self()},
    receive
        X -> X
    end.

loop() ->
    receive
        {init, Name, Options, Sender} ->
            try
                catch ets:delete(Name),
                Name = ets:new(Name, [named_table, public | Options]),
                Sender ! ok,
                loop()
            catch
                _:E -> Sender ! {error, E}
            end;
        stop -> ok
    end.

-spec(keys(atom()) -> [term()]).
keys(Name) -> lists:flatten(ets:match(Name, {'$1', '_'})).

-spec(get(atom(), term()) -> option_m:monad([term()])).
get(Name, Key) ->
    case ets:lookup(Name, Key) of
        [] -> undefined;
        List -> {ok, lists:map(fun({_Key, Value}) -> Value end, List)}
    end.

-spec(value(atom(), term()) -> option_m:monad(term())).
value(Name, Key) ->
    case get(Name, Key) of
        {ok, [V]} -> {ok, V};
        _ -> undefined
    end.

-spec(evalue(atom(), term()) -> error_m:monad(term())).
evalue(Name, Key) -> option_m:to_error_m(value(Name, Key), {no, Key}).

-spec(set(atom(), term(), term()) -> ok).
set(Name, Key, Value) ->
    true = ets:insert(Name, {Key, Value}),
    ok.

-spec(increment(atom(), term()) -> integer()).
increment(Name, Key) -> increment(Name, Key, 1).

-spec(increment(atom(), term(), integer()) -> integer()).
increment(Name, Key, Value) -> ets:update_counter(Name, Key, Value).

-spec(delete(atom()) -> ok).
delete(Name) ->
    catch (ets:delete(Name)),
    ok.

-spec(remove(atom(), term()) -> ok).
remove(Name, Key) ->
    true = ets:delete(Name, Key),
    ok.

