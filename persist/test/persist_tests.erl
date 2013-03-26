-module(persist_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_stdlib/include/xl_eunit.hrl").

-record(testobj, {
    id,
    name
}).

persist_test() ->
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
    persist:open(test, persist:by_index(#testobj.id),
        persist_storage_bin:new("/tmp/test/test"), [
            {fsync_interval, 100},
            {indices, [{name, fun(#testobj{id = Id, name = Name}) -> [{Name, Id, Id}] end}]}
        ]).

cursor_test() ->
    {ok, P} = open(),
    L = [_ | T] = lists:map(fun(I) -> #testobj{id = integer_to_list(I), name = "n1"} end, lists:seq(1, 9)),

    [persist:store(P, X) || X <- L],

    lists:foreach(fun(_) ->
        ?assertEqual(length(L), length(xl_stream:to_list(persist:cursor(P, [random]))))
    end, lists:seq(1, 100)),

    ?assertNotEqual(xl_stream:to_list(persist:cursor(P, [random])), xl_stream:to_list(persist:cursor(P, [random]))),

    Compare = fun(#testobj{id = Id1}, #testobj{id = Id2}) -> Id1 < Id2 end,
    ?assertEqual(L, lists:sort(Compare, xl_stream:to_list(persist:cursor(P, [random])))),

    persist:delete(P, "1"),
    ?assertEquals(T, xl_stream:to_list(persist:cursor(P))),

    ?assertEqual(ok, persist:close(P)).


indexed_lookup_test() ->
    {ok, P} = open(),
    L = lists:map(fun(I) -> #testobj{id = integer_to_list(I), name = integer_to_list(I rem 3)} end, lists:seq(1, 9)),

    [persist:store(P, X) || X <- L],

    ?assertEqual([
        {#testobj{id = "2", name = "2"}, [{"2", "2", "2"}]},
        {#testobj{id = "5", name = "2"}, [{"2", "5", "5"}]},
        {#testobj{id = "8", name = "2"}, [{"2", "8", "8"}]}
    ], xl_stream:to_list(persist:lookup(P, [{name, "2"}]))),

    ?assertEqual(ok, persist:close(P)).
