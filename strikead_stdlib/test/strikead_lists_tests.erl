-module(strikead_lists_tests).

-include_lib("eunit/include/eunit.hrl").

find_test() ->
    L = [1,2,3,4],
    ?assertEqual({ok, 2}, strikead_lists:find(fun(X) -> X == 2 end, L)),
    ?assertEqual(nothing, strikead_lists:find(fun(X) -> X == 0 end, L)).

emap_test() ->
	L1 = [{ok, 1}, {ok, 2}, {ok, 3}],
    ?assertEqual({ok, [1, 2, 3]},
		strikead_lists:emap(fun(X) -> X end, L1)),
	L2 = [{ok, 1}, {error, nope}, {ok, 3}],
    ?assertEqual({error, nope},
		strikead_lists:emap(fun(X) -> X end, L2)).

mapfilter_test() ->
	L = [1, 2, 3, 4, 5],
	?assertEqual([4, 16],
		strikead_lists:mapfilter(
			fun(X) when X rem 2 == 0 -> {ok, X * X}; (_) -> nothing end, L)).

keypsort_test() ->
	?assertEqual([{z, 1},{x, 2},{y, 3}],
		strikead_lists:keypsort([z,x,y], 1, [{x, 2},{z, 1},{y, 3}])).
