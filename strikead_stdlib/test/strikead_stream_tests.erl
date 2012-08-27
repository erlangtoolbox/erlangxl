-module(strikead_stream_tests).

-include_lib("eunit/include/eunit.hrl").

to_stream_test() ->
    L = [1, 2, 3, 4],
    ?assertEqual(L, strikead_stream:to_list(strikead_stream:to_stream(L))).

mapfind_test() ->
    ?assertEqual({ok, {6, x}}, strikead_stream:mapfind(
        fun
                (3) -> {ok, {6, x}};
                (_) -> undefined
        end, strikead_stream:seq(1, 10))).

mapfind_not_found_test() ->
    ?assertEqual(undefined, strikead_stream:mapfind(fun(_) -> undefined end, strikead_stream:seq(1, 10))).

foldl_test() ->
    Sum = fun(X, Acc) -> X + Acc end,
    ?assertEqual(lists:foldl(Sum, 0, lists:seq(1, 5)), strikead_stream:foldl(Sum, 0, strikead_stream:seq(1, 5))).

