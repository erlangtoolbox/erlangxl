%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(xl_lists_tests).

-include_lib("eunit/include/eunit.hrl").
-include("xl_eunit.hrl").

find_test() ->
    L = [1, 2, 3, 4],
    ?assertEqual({ok, 2}, xl_lists:find(fun(X) -> X == 2 end, L)),
    ?assertEqual(undefined, xl_lists:find(fun(X) -> X == 0 end, L)).

emap_test() ->
    L1 = [{ok, 1}, {ok, 2}, {ok, 3}],
    ?assertEqual({ok, [1, 2, 3]}, xl_lists:emap(fun(X) -> X end, L1)),
    L2 = [{ok, 1}, {error, nope}, {ok, 3}],
    ?assertEqual({error, nope}, xl_lists:emap(fun(X) -> X end, L2)).

eflatmap_test() ->
    L1 = [{ok, [1]}, {ok, [2]}, {ok, [3]}],
    ?assertEqual({ok, [1, 2, 3]}, xl_lists:eflatmap(fun(X) -> X end, L1)),
    L2 = [{ok, [1]}, {error, nope}, {ok, [3]}],
    ?assertEqual({error, nope}, xl_lists:eflatmap(fun(X) -> X end, L2)).

eforeach_test() ->
    L1 = [{ok, 1}, {ok, 2}, {ok, 3}],
    ?assertEqual(ok, xl_lists:eforeach(fun(X) -> X end, L1)),
    L2 = [{ok, 1}, {error, nope}, {ok, 3}],
    ?assertEqual({error, nope}, xl_lists:eforeach(fun(X) -> X end, L2)).

mapfilter_test() ->
    L = [1, 2, 3, 4, 5],
    ?assertEqual([4, 16], xl_lists:mapfilter(fun(X) when X rem 2 == 0 -> {ok, X * X}; (_) -> undefined end, L)).

mapfind_test() ->
    L = [1, 2, 3, 4, 5],
    ?assertEqual({ok, 4}, xl_lists:mapfind(fun(X) when X rem 2 == 0 -> {ok, X * X}; (_) -> undefined end, L)).

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
    xl_eunit:performance(regex, fun() -> xl_lists:sublistmatch(Pattern, List) end, Count),
    Pattern2 = [{a, 1}, {b, not_empty}],
    xl_eunit:performance(atom, fun() -> xl_lists:sublistmatch(Pattern2, List) end, Count).

substitute_test() ->
    ?assertOk(xl_application:start(xl_stdlib)),
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
        xl_lists:keyreplace_or_add(1, [{z, 3}], [{x, 1}, {y, 2}])),
    ?assertEqual([{x, 1}, {z, 1}, {z, 3}],
        xl_lists:keyreplace_or_add(1, [{x, a}, {z, 1}, {z, 3}], [{x, 1}])).


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
    ?assert(C1 >= 20),
    ?assert(C2 >= 20),
    ?assert(C3 >= 20),
    Results2 = [xl_lists:random([1, 2]) || _ <- lists:seq(1, 100)],
    [{_, C21}, {_, C22}] = xl_lists:count_unique(Results2),
    ?assert(C21 >= 30),
    ?assert(C22 >= 30).

split_by_test() ->
    ?assertEqual([[1, 2], [4, 4, 5], [5, 6]], xl_lists:split_by([1, 2, 3, 4, 4, 5, 3, 5, 6], 3)),
    ?assertEqual([[], [4, 4, 5], []], xl_lists:split_by([3, 4, 4, 5, 3], 3)).

efoldl_test() ->
    F = fun(3, _) -> {error, 3}; (X, A) -> {ok, X + A} end,
    ?assertEqual({ok, 3}, xl_lists:efoldl(F, 0, [1, 1, 1])),
    ?assertEqual({error, 3}, xl_lists:efoldl(F, 0, [1, 3, 1])).

imap_test() ->
    ?assertEqual([{a, 1}, {b, 2}, {c, 3}], xl_lists:imap(fun(X, I) -> {X, I} end, [a, b, c])).

zip_with_index_test() ->
    ?assertEqual([{a, 1}, {b, 2}, {c, 3}], xl_lists:zip_with_index([a, b, c])).

intersect_test() ->
    ?assertEqual([b, a], xl_lists:intersect([a, b, c], [a, b, d])),
    ?assertEqual([b, a], xl_lists:intersect([a, b, b, c], [a, b, d, d])),
    ?assertEqual([b, a], xl_lists:intersect([a, b, d, d], [a, b, b, c])).

union_test() ->
    ?assertEqual([d, c, b, a], xl_lists:union([a, b, c], [a, b, d])),
    ?assertEqual([d, c, b, a], xl_lists:union([a, b, b, c], [a, b, d, d])),
    ?assertEqual([d, c, b, a], xl_lists:union([a, b, d, d], [a, b, b, c])).

transform_gb_tree_test() ->
    Expected = gb_trees:insert(3, c, gb_trees:insert(2, b, gb_trees:insert(1, a, gb_trees:empty()))),
    ?assertEqual(Expected, xl_lists:transform(gb_tree, fun(X) -> X end, [{1, a}, {2, b}, {3, c}])).

transform_dict_test() ->
    Expected = dict:append([1, 2], x, dict:append([1, 2], y, dict:append([3, 3], z, dict:new()))),
    ?assertEqual(Expected, xl_lists:transform(dict, fun(X) -> X end, [{[3, 3], z}, {[1, 2], y}, {[1, 2], x}])).

gb_tree_vs_random_access_list_test() ->
    Counts = xl_lists:seq(1, 10, 0.5, fun(X) -> round(math:exp(X)) end),
    lists:foreach(fun(Count) ->
        T = lists:map(fun(X) -> {X, X} end, lists:seq(1, Count)),
        xl_eunit:performance(xl_convert:make_atom([lists_vs_gb_trees_list, '#', Count]), fun() ->
            {ok, _} = xl_lists:keyfind(random:uniform(Count), 1, T)
        end, 1000)
    end, Counts),
    lists:foreach(fun(Count) ->
        T = xl_lists:transform(gb_tree, fun(X) -> {X, X} end, lists:seq(1, Count)),
        xl_eunit:performance(xl_convert:make_atom([lists_vs_gb_trees_tree, '#', Count]), fun() ->
            {value, _} = gb_trees:lookup(random:uniform(Count), T)
        end, 1000)
    end, Counts).

matchfilter_test() ->
    ?assertEqual([[{3, 1}, {3, 3}, {3, 2}], [{7, 1}, {7, 2}, {7, 3}]], xl_lists:matchfilter(fun xl_lists:compare_key/2, [
        [{1, 1}, {3, 1}, {7, 1}, {8, 1}],
        [{3, 2}, {7, 2}, {8, 2}],
        [{1, 3}, {2, 3}, {3, 3}, {5, 3}, {7, 3}]
    ])),
    ?assertEqual([[{1, 1}], [{3, 1}], [{7, 1}], [{8, 1}]], xl_lists:matchfilter(fun xl_lists:compare_key/2, [[{1, 1}, {3, 1}, {7, 1}, {8, 1}]])),
    ?assertEqual([], xl_lists:matchfilter(fun xl_lists:compare_key/2, [[], []])),
    ?assertEqual([[1], [3]], xl_lists:matchfilter(fun xl_lists:compare/2, [[1, 3]])).

nth_test() ->
    ?assertEqual({ok, 2}, xl_lists:nth(2, [1, 2, 3])),
    ?assertEqual(undefined, xl_lists:nth(22, [1, 2, 3])).

keymerge_test() ->
    Plus = fun({K, V1}, {_, V2}) -> {K, V1 + V2} end,
    ?assertEqual([{a, 2}, {b, 4}, {c, 5}], xl_lists:keymerge(Plus, 1, [{a, 2}, {b, 3}], [{b, 1}, {c, 5}])),
    ?assertEqual([{b, 1}, {c, 5}], xl_lists:keymerge(Plus, 1, [], [{b, 1}, {c, 5}])),
    ?assertEqual([{b, 1}, {c, 5}], xl_lists:keymerge(Plus, 1, [{b, 1}, {c, 5}], [])).

shuffle_test() ->
    ?assertEqual([], xl_lists:shuffle([])),
    L = lists:seq(1, 1000),
    ?assertNotEqual(xl_lists:shuffle(L), xl_lists:shuffle(L)),
    Counts = xl_lists:seq(1, 5, 0.5, fun(X) -> round(math:exp(X)) end),
    lists:foreach(fun(Count) ->
        T = lists:map(fun(X) -> {X, X} end, lists:seq(1, Count)),
        xl_eunit:performance(xl_convert:make_atom([shuffle, '#', Count]), fun() ->
            xl_lists:shuffle(T)
        end, 1000)
    end, Counts).


nshufflemapfilter_test() ->
    L = lists:seq(1, 100),
    xl_lists:times(fun() ->
        R = xl_lists:nshufflemapfilter(10, fun(X) when X rem 2 == 0 -> {ok, X}; (_) -> undefined end, L),
        ?assertEqual(10, length(R)),
        lists:foreach(fun(E) -> ?assertEqual(0, E rem 2) end, R)
    end, 10).

nshufflemapfilter_random_test() ->
    L = lists:seq(1, 100),
    Unique = xl_lists:count_unique(lists:map(fun(_) ->
        [H | _] = xl_lists:nshufflemapfilter(100, fun(X) -> {ok, X} end, L),
        H
    end, lists:seq(1, 10))),
    ?assert(length(Unique) > 5).

delete_all_test() ->
    ?assertEqual([1, 2, 3, 4], xl_lists:delete_all(x, [1, x, 2, x, x, 3, 4])).

set_test() ->
    L = [random:uniform(1000) || _ <- lists:seq(1, 1000)],
    ?assertEquals(length(xl_lists:set(L)), length(sets:to_list(sets:from_list(L)))),
    xl_lists:times(fun() ->
        xl_eunit:performance(xl_lists_set, fun() ->
            xl_lists:set(L)
        end, 100)
    end, 5),
    xl_lists:times(fun() ->
        xl_eunit:performance(sets_from_list, fun() ->
            sets:to_list(sets:from_list(L))
        end, 100)
    end, 5).

