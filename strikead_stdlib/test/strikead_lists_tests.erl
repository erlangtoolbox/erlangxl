-module(strikead_lists_tests).

-include_lib("eunit/include/eunit.hrl").

find_test() ->
    L = [1,2,3,4],
    ?assertEqual({ok, 2}, strikead_lists:find(fun(X) -> X == 2 end, L)),
    ?assertEqual(nothing, strikead_lists:find(fun(X) -> X == 0 end, L)).

emap_test() ->
    ?assertEqual({ok, [1,2,3]}, strikead_lists:emap(fun(X) -> X end, [{ok, 1}, {ok, 2}, {ok, 3}])),
    ?assertEqual({error, nope}, strikead_lists:emap(fun(X) -> X end, [{ok, 1}, {error, nope}, {ok, 3}])).

mapfilter_test() ->
	?assertEqual([4,16], strikead_lists:mapfilter(fun(X) when X rem 2 == 0 -> {ok, X * X}; (_) -> nothing end, [1,2,3,4,5])).
