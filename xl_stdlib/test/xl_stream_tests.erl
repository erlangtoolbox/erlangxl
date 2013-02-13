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
    L = lists:seq(1, 10),
    R = xl_stream:to_list(xl_stream:to_random_stream(L)),
    ?assertNotEqual(L, R),
    ?assertEqual(L, lists:sort(R)).

keyfilter_test() ->
    Keys = [1, 3, 6],
    Objects = xl_stream:to_stream(lists:zip(lists:seq(1, 7), lists:seq(1, 7))),
    ?assertEqual([{1, 1}, {3, 3}, {6, 6}], xl_stream:to_list(xl_stream:keyfilter(Keys, 1, Objects))).
