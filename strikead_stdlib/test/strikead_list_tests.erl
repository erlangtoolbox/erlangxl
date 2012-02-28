-module(strikead_list_tests).

-include_lib("eunit/include/eunit.hrl").

find_test() ->
    L = [1,2,3,4],
    ?assertEqual({ok, 2}, strikead_list:find(fun(X)-> X == 2 end, L)),
    ?assertEqual(not_found, strikead_list:find(fun(X)-> X == 0 end, L)).

