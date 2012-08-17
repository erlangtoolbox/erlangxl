-module(persist_tests).

-include_lib("eunit/include/eunit.hrl").

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
            {fsync_interval, 100}
        ]).
