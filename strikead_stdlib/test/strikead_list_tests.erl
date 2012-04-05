-module(strikead_list_tests).

-include_lib("eunit/include/eunit.hrl").

find_test() ->
    L = [1,2,3,4],
    ?assertEqual({ok, 2}, strikead_list:find(fun(X) -> X == 2 end, L)),
    ?assertEqual(not_found, strikead_list:find(fun(X) -> X == 0 end, L)).

emap_test() ->
    ?assertEqual({ok, [1,2,3]}, strikead_list:emap(fun(X) -> X end, [{ok, 1}, {ok, 2}, {ok, 3}])),
    ?assertEqual({error, nope}, strikead_list:emap(fun(X) -> X end, [{ok, 1}, {error, nope}, {ok, 3}])).

