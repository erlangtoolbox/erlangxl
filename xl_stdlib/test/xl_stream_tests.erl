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
-module(xl_stream_tests).

-include_lib("eunit/include/eunit.hrl").

to_stream_test() ->
    L = [1, 2, 3, 4],
    ?assertEqual(L, xl_stream:to_list(xl_stream:to_stream(L))).

mapfind_test() ->
    ?assertEqual({ok, {6, x}}, xl_stream:mapfind(
        fun
            (3) -> {ok, {6, x}};
            (_) -> undefined
        end, xl_stream:seq(1, 10))).

mapfind_not_found_test() ->
    ?assertEqual(undefined, xl_stream:mapfind(fun(_) -> undefined end, xl_stream:seq(1, 10))).

foldl_test() ->
    Sum = fun(X, Acc) -> X + Acc end,
    ?assertEqual(lists:foldl(Sum, 0, lists:seq(1, 5)), xl_stream:foldl(Sum, 0, xl_stream:seq(1, 5))).

to_random_stream_test() ->
    L = lists:seq(1, 100),
    ?assertNotEqual(xl_stream:to_list(xl_stream:to_random_stream(L)), xl_stream:to_list(xl_stream:to_random_stream(L))),
    ?assertEqual(L, lists:sort(xl_stream:to_list(xl_stream:to_random_stream(L)))).

keyfilter_test() ->
    Keys = [1, 3, 6],
    Objects = xl_stream:to_stream(lists:zip(lists:seq(1, 7), lists:seq(1, 7))),
    ?assertEqual([{1, 1}, {3, 3}, {6, 6}], xl_stream:to_list(xl_stream:keyfilter(Keys, 1, Objects))).

to_rpc_stream_test() ->
    ?assertEqual([{ok, 1}, {ok, 2}, {ok, 3}], xl_stream:to_list(xl_stream:to_rpc_stream(xl_stream:to_stream([1, 2, 3])))).

matchfilter_test() ->
    ?assertEqual([[{3, 1}, {3, 3}, {3, 2}], [{7, 1}, {7, 2}, {7, 3}]], xl_stream:to_list(
        xl_stream:matchfilter(fun xl_lists:compare_key/2, [
            [{1, 1}, {3, 1}, {7, 1}, {8, 1}],
            [{3, 2}, {7, 2}, {8, 2}],
            [{1, 3}, {2, 3}, {3, 3}, {5, 3}, {7, 3}]
        ])
    )),
    ?assertEqual([[{1, 1}], [{3, 1}], [{7, 1}], [{8, 1}]],
        xl_stream:to_list(xl_stream:matchfilter(fun xl_lists:compare_key/2, [[{1, 1}, {3, 1}, {7, 1}, {8, 1}]]))),
    ?assertEqual([], xl_stream:to_list(xl_stream:matchfilter(fun xl_lists:compare_key/2, [[], []]))).

concat_test() ->
    ?assertEqual([1, 2, 3, 4, 5, 6], xl_stream:to_list(xl_stream:concat([
        xl_stream:to_stream([1, 2]),
        xl_stream:to_stream([3]),
        xl_stream:to_stream([4, 5, 6])
    ]))).

listn_test() ->
    R = xl_stream:listn(3, xl_stream:to_stream(lists:seq(1, 10))),
    xl_eunit:format("~p~n", [R]),
    ?assertEqual([[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]],
        xl_stream:to_list(R)),
    ?assertEqual([[1, 2]], xl_stream:to_list(xl_stream:listn(3, xl_stream:to_stream([1, 2])))),
    ?assertEqual([], xl_stream:to_list(xl_stream:listn(3, xl_stream:empty()))).
