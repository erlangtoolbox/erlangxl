-module(xl_lists_tests).

-include_lib("eunit/include/eunit.hrl").

find_test() ->
    L = [1, 2, 3, 4],
    ?assertEqual({ok, 2}, xl_lists:find(fun(X) -> X == 2 end, L)),
    ?assertEqual(undefined, xl_lists:find(fun(X) -> X == 0 end, L)).

emap_test() ->
    L1 = [{ok, 1}, {ok, 2}, {ok, 3}],
    ?assertEqual({ok, [1, 2, 3]},
        xl_lists:emap(fun(X) -> X end, L1)),
    L2 = [{ok, 1}, {error, nope}, {ok, 3}],
    ?assertEqual({error, nope},
        xl_lists:emap(fun(X) -> X end, L2)).

eforeach_test() ->
    L1 = [{ok, 1}, {ok, 2}, {ok, 3}],
    ?assertEqual(ok,
        xl_lists:eforeach(fun(X) -> X end, L1)),
    L2 = [{ok, 1}, {error, nope}, {ok, 3}],
    ?assertEqual({error, nope},
        xl_lists:eforeach(fun(X) -> X end, L2)).

mapfilter_test() ->
    L = [1, 2, 3, 4, 5],
    ?assertEqual([4, 16],
        xl_lists:mapfilter(
            fun(X) when X rem 2 == 0 -> {ok, X * X}; (_) -> undefined end, L)).

keypsort_test() ->
    ?assertEqual([{z, 1}, {x, 2}, {y, 3}],
        xl_lists:keypsort([z, x, y], 1, [{x, 2}, {z, 1}, {y, 3}])).

sublistmatch_test() ->
    Pattern = [{a, 1}, {b, "x.*"}],
    List = [{a, 1}, {x, y}, {b, "xx"}],
    ?assert(xl_lists:sublistmatch(Pattern, List)),
    List2 = [{x, y}, {b, "xx"}],
    ?assertNot(xl_lists:sublistmatch(Pattern, List2)),
    List3 = [{a, 1}, {x, y}, {b, "yy"}],
    ?assertNot(xl_lists:sublistmatch(Pattern, List3)).

sublistmatch_perf_test() ->
    Pattern = [{a, 1}, {b, ".+"}],
    List = [{a, 1}, {x, y}, {b, "aaa"}],
    Count = 100000,
    xl_eunit:performance(regex, fun(_) -> xl_lists:sublistmatch(Pattern, List) end, Count),
    Pattern2 = [{a, 1}, {b, not_empty}],
    xl_eunit:performance(atom, fun(_) -> xl_lists:sublistmatch(Pattern2, List) end, Count).

substitute_test() ->
    Pattern = [a, 1, "c{b}c", "c"],
    List = [{a, "1"}, {b, "x"}, {"c", 2}],
    ?assertEqual(["1", 1, "cxc", 2],
        xl_lists:substitute(Pattern, List,
            fun xl_string:substitute/2)).


keyfind_test() ->
    ?assertEqual({x, y}, xl_lists:keyfind(z, 1, [], {x, y})).

kvfind_test() ->
    ?assertEqual({ok, v}, xl_lists:kvfind(k, [{k, v}])),
    ?assertEqual(undefined, xl_lists:kvfind(z, [{k, v}])),
    ?assertEqual(y, xl_lists:kvfind(z, [], y)).

keyreplace_test() ->
    ?assertEqual([{x, 1}, {y, 2}, {z, 3}],
        xl_lists:keyreplace(1, [{x, a}, {y, b}, {z, 3}], [{x, 1}, {y, 2}])).

keyreplace_or_add_test() ->
    ?assertEqual([{x, 1}, {z, 3}],
        xl_lists:keyreplace_or_add(1, [{x, a}, {z, 3}], {x, 1})),
    ?assertEqual([{x, 1}, {z, 3}],
        xl_lists:keyreplace_or_add(1, [{z, 3}], {x, 1})),
    ?assertEqual([{y, 2}, {x, 1}, {z, 3}],
        xl_lists:keyreplace_or_add(1, [{z, 3}], [{x, 1}, {y, 2}])).

split_test() ->
    ?assertEqual({[1, 2], [3, 4]}, xl_lists:split(2, [1, 2, 3, 4])),
    ?assertEqual({[1, 2], []}, xl_lists:split(2, [1, 2])),
    ?assertEqual({[1], []}, xl_lists:split(2, [1])).


insert_before_test() ->
    ?assertEqual([1, 2, 3], xl_lists:insert_before(3, 2, [1, 3])),
    ?assertEqual([one, two, three, four, four], xl_lists:insert_before(four, three, [one, two, four, four])).


random_test() ->
    Results = [xl_lists:random([1, 2, 3]) || _ <- lists:seq(1, 100)],
    [{_, C1}, {_, C2}, {_, C3}] = xl_lists:count_unique(Results),
    ?assert(C1 >= 25),
    ?assert(C2 >= 25),
    ?assert(C3 >= 25),
    Results2 = [xl_lists:random([1, 2]) || _ <- lists:seq(1, 100)],
    [{_, C21}, {_, C22}] = xl_lists:count_unique(Results2),
    ?assert(C21 >= 40),
    ?assert(C22 >= 40).

split_by_test() ->
    ?assertEqual([[1, 2], [4, 4, 5], [5, 6]], xl_lists:split_by([1, 2, 3, 4, 4, 5, 3, 5, 6], 3)),
    ?assertEqual([[], [4, 4, 5], []], xl_lists:split_by([3, 4, 4, 5, 3], 3)).

efoldl_test() ->
    F = fun(3, _) -> {error, 3}; (X, A) -> {ok, X + A} end,
    ?assertEqual({ok, 3}, xl_lists:efoldl(F, 0, [1, 1, 1])),
    ?assertEqual({error, 3}, xl_lists:efoldl(F, 0, [1, 3, 1])).

imap_test() ->
    ?assertEqual([{a, 1}, {b, 2}, {c, 3}], xl_lists:imap(fun(X, I) -> {X, I} end, [a, b, c])).