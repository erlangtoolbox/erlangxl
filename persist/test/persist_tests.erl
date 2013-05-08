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
-module(persist_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_stdlib/include/xl_eunit.hrl").

-record(testobj, {
    id,
    name
}).

persist_test() ->
    xl_file:delete("/tmp/test/test"),
    {ok, P} = open(),
    T1 = #testobj{id = "1", name = "n1"},
    T2 = #testobj{id = "2", name = "n2"},

    persist:store(P, T1),
    persist:store(P, T2),
    ?assertEqual([T1, T2], persist:select(P)),
    ?assertEqual({ok, T1}, persist:get(P, "1")),
    ?assertEqual({ok, T2}, persist:get(P, "2")),
    ?assertEqual(undefined, persist:get(P, "3")),

    persist:delete(P, "1"),
    ?assertEqual([T2], persist:select(P)),
    ?assertEqual(undefined, persist:get(P, "1")),
    ?assertEqual({ok, T2}, persist:get(P, "2")),

    ?assertEqual(ok, persist:close(P)).


persiste_fsync_test() ->
    xl_file:delete("/tmp/test/test"),
    {ok, P1} = open(),
    T1 = #testobj{id = "1", name = "n1"},
    T2 = #testobj{id = "2", name = "n2"},
    persist:store(P1, T1),
    persist:store(P1, T2),
    timer:sleep(500),
    ?assertEqual(ok, persist:close(P1)),
    {ok, P2} = open(),
    ?assertEqual([T1, T2], persist:select(P2)),
    ?assertEqual(ok, persist:close(P2)).

open() ->
    xl_application:start(xl_stdlib),
    persist:open(test, persist:by_index(#testobj.id),
        persist_storage_bin:new("/tmp/test/test"), [
            {fsync_interval, 100},
            {indices, [
                {name, fun(#testobj{id = Id, name = Name}) -> [{Name, Id, Id}] end},
                {name2, fun(#testobj{id = Id, name = Name}) -> [{Name, Id, Id}] end}
            ]}
        ]).

cursor_test() ->
    xl_file:delete("/tmp/test/test"),
    {ok, P} = open(),
    L = [_ | T] = lists:map(fun(I) -> #testobj{id = xl_string:format_number(2, I), name = "n1"} end, lists:seq(1, 99)),

    [persist:store(P, X) || X <- L],

    lists:foreach(fun(_) ->
        ?assertEquals(length(L), length(xl_stream:to_list(persist:cursor(P, [random]))))
    end, lists:seq(1, 100)),

    ?assertNotEqual(xl_stream:to_list(persist:cursor(P, [random])), xl_stream:to_list(persist:cursor(P, [random]))),

    Compare = fun(#testobj{id = Id1}, #testobj{id = Id2}) -> Id1 < Id2 end,
    ?assertEquals(L, lists:sort(Compare, xl_stream:to_list(persist:cursor(P, [random])))),

    persist:delete(P, "01"),
    ?assertEquals(T, xl_stream:to_list(persist:cursor(P))),

    ?assertEquals(ok, persist:close(P)).


indexed_lookup_test() ->
    xl_file:delete("/tmp/test/test"),
    {ok, P} = open(),
    L = lists:map(fun(I) -> #testobj{id = integer_to_list(I), name = integer_to_list(I rem 3)} end, lists:seq(1, 9)),

    [persist:store(P, X) || X <- L],

    ?assertEqual([
        {#testobj{id = "2", name = "2"}, [{name, "2"}, {name2, "2"}]},
        {#testobj{id = "5", name = "2"}, [{name, "5"}, {name2, "5"}]},
        {#testobj{id = "8", name = "2"}, [{name, "8"}, {name2, "8"}]}
    ], xl_stream:to_list(persist:lookup(P, [{name, "2"}, {name2, "2"}]))),

    ?assertEqual(ok, persist:close(P)).
