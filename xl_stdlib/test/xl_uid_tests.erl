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
-module(xl_uid_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

-export([gen/2]).

next_test_() ->
    {timeout, 2000, fun() ->
        application:start(xl_stdlib),
        ?assert(xl_uid:next() < xl_uid:next()),
        xl_eunit:performance(uid, fun() -> xl_uid:next() end, 100000),
        xl_eunit:performance(uid_hex, fun() -> xl_uid:next_hex() end, 100000)
    end}.

uniq_test() ->
    {timeout, 2000, fun() ->
        application:start(xl_stdlib),
        xl_state:new(state1),
        lists:foreach(fun(X) ->
            spawn(?MODULE, gen, [state1, xl_convert:make_atom([result, X])])
        end, lists:seq(1, 100)),
        timer:sleep(1000),
        [H | T] = xl_state:keys(state1),
        L = lists:foldl(
            fun(Current, L) -> lists:subtract(L, element(2, xl_state:value(state1, Current))) end,
            element(2, xl_state:value(state1, H)),
            T
        ),
        ?assertEqual(1000, length(L))
    end}.

gen(State, Name) ->
    L = [xl_uid:next() || _ <- lists:seq(1, 1000)],
    xl_state:set(State, Name, L).
