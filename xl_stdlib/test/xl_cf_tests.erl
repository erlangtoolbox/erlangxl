%% Copyright
-module(xl_cf_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").
-include("xl_cf.hrl").

lists_test() ->
    ?assertEqual([{X, Y, Z} || X <- [1, 2, 3], Y <- [a, b, c], Z <- [x, y, z]],
        xl_cf:flatmap([{X, Y, Z} || X <- [1, 2, 3], Y <- [a, b, c], Z <- [x, y, z]])).

stream_test() ->
    ?assertEqual([{X, Y, Z} || X <- [1, 2, 3], Y <- [a, b, c], Z <- [x, y, z]],
        xl_stream:to_list(xl_cf:flatmap([{X, Y, Z} ||
            X <- xl_stream:to_stream([1, 2, 3]),
            Y <- xl_stream:to_stream([a, b, c]),
            Z <- xl_stream:to_stream([x, y, z])
        ]))).

gb_tree_test() ->
    ?assertEqual(gb_trees:insert(1, 2, gb_trees:insert(2, 4, gb_trees:insert(3, 6, gb_trees:empty()))),
        xl_cf:flatmap([V + K ||
            {K, V} <- gb_trees:insert(1, 1, gb_trees:insert(2, 2, gb_trees:insert(3, 3, gb_trees:empty())))
        ])).
