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
    Cmp = fun
        ({X, _}, {X, _}) -> eq;
        ({X, _}, {Y, _}) when X > Y -> gt;
        (_, _) -> lt
    end,
    ?assertEqual([[{3, 1}, {3, 3}, {3, 2}], [{7, 1}, {7, 2}, {7, 3}]], xl_stream:to_list(
        xl_stream:matchfilter(Cmp, [
            [{1, 1}, {3, 1}, {7, 1}, {8, 1}],
            [{3, 2}, {7, 2}, {8, 2}],
            [{1, 3}, {2, 3}, {3, 3}, {5, 3}, {7, 3}]
        ])
    )),
    ?assertEqual([[{1, 1}], [{3, 1}], [{7, 1}], [{8, 1}]], xl_stream:to_list(xl_stream:matchfilter(Cmp, [[{1, 1}, {3, 1}, {7, 1}, {8, 1}]]))),
    ?assertEqual([], xl_stream:to_list(xl_stream:matchfilter(Cmp, [[], []]))).

concat_test() ->
    ?assertEqual([1, 2, 3, 4, 5, 6], xl_stream:to_list(xl_stream:concat([
        xl_stream:to_stream([1, 2]),
        xl_stream:to_stream([3]),
        xl_stream:to_stream([4, 5, 6])
    ]))).


