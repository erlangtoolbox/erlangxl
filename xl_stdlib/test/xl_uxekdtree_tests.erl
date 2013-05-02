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
-module(xl_uxekdtree_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").
-include("xl_eunit.hrl").

-define(POINTS_FOR_SMALL_TREE, [
    {1, c, c}, {1, b, b}, {3, a, a}, {2, c, c}, {1, b, b}, {3, a, a}, {2, c, c}, {1, b, b}, {3, a, a}, {2, c, c}
]).
-define(POINTS_FOR_SMALL_TREE_WITH_UNDEFS, [
    {1, c, c}, {undefined, b, ub1}, {3, a, a}, {2, undefined, uc}, {undefined, b, ub2},
    {3, a, a}, {2, c, c}, {1, b, b}, {3, a, a}, {2, c, c}
]).

-define(POINTS_FOR_SMALL_TREE_WITH_EXCLUDES, [
    {1, c, c}, {{x, 1}, b, xb1}, {3, a, a}, {2, {x, c}, xc}, {{x, 2}, b, xb2},
    {3, a, a}, {2, c, c}, {1, b, b}, {3, a, a}, {2, c, c}
]).

new_test() ->
    xl_application:start(xl_stdlib),
    ExpectedTree = {xl_uxekdtree,
        {2, 1,
            [],
            {b, 2,
                [],
                [],
                {1, 1, [], [], [b, b, b], [], []},
                {1, 1,
                    [],
                    [],
                    {c, 2, [], [], [c], [], []},
                    [],
                    []
                },
                []
            },
            {c, 2, [], [], [c, c, c], [], []},
            {a, 2,
                [],
                [],
                {3, 1, [], [], [a, a, a], [], []},
                [],
                []
            },
            []
        }, []},
    ?assertEquals(ExpectedTree, xl_uxekdtree:new(?POINTS_FOR_SMALL_TREE)).

new_with_excludes_test() ->
    xl_application:start(xl_stdlib),
    ExpectedTree = {xl_uxekdtree,
        {2, 1,
            [],
            {b, 2,
                [],
                [],
                {1, 1, [], [], [b], [], [xb1]},
                {1, 1,
                    [],
                    [],
                    {c, 2, [], [], [c], [], []},
                    [],
                    []
                },
                []
            },
            {c, 2, [], [], [c, c], [], [xc]},
            {a, 2,
                [],
                [],
                {3, 1, [], [], [a, a, a], [], []},
                [],
                []
            },
            {b, 2, [], [], [xb2], [], []}
        }, []},
    ?assertEquals(ExpectedTree, xl_uxekdtree:new(?POINTS_FOR_SMALL_TREE_WITH_EXCLUDES)).

new_with_undefs_test() ->
    xl_application:start(xl_stdlib),
    ExpectedTree = {xl_uxekdtree,
        {2, 1,
            {b, 2, [], [], [ub2, ub1], [], []},
            {b, 2,
                [],
                [],
                {1, 1, [], [], [b], [], []},
                {1, 1,
                    [],
                    [],
                    {c, 2, [], [], [c], [], []},
                    [],
                    []
                },
                []
            },
            {c, 2, [uc], [], [c, c], [], []},
            {a, 2,
                [],
                [],
                {3, 1, [], [], [a, a, a], [], []},
                [],
                []
            },
            []
        }, []},
    ?assertEquals(ExpectedTree, xl_uxekdtree:new(?POINTS_FOR_SMALL_TREE_WITH_UNDEFS)).

new_performance_test_() ->
    xl_application:start(xl_stdlib),
    {timeout, 2000, fun() ->
        xl_eunit:performance(uxekdtree, fun() ->
            prepare_space(10, 1000)
        end, 5)
    end}.

find_all_test() ->
    xl_application:start(xl_stdlib),
    Tree = xl_uxekdtree:new(?POINTS_FOR_SMALL_TREE),
    Q = {undefined, undefined},
    Expected = lists:sort(lists:map(fun({_, _, V}) -> V end, ?POINTS_FOR_SMALL_TREE)),
    ?assertEquals(Expected, lists:sort(element(2, xl_uxekdtree:find(Q, Tree)))).

find_all_with_undefs_test() ->
    xl_application:start(xl_stdlib),
    Tree = xl_uxekdtree:new(?POINTS_FOR_SMALL_TREE_WITH_UNDEFS),
    Q = {undefined, undefined},
    Expected = lists:sort(lists:map(fun({_, _, V}) -> V end, ?POINTS_FOR_SMALL_TREE_WITH_UNDEFS)),
    ?assertEquals(Expected, lists:sort(element(2, xl_uxekdtree:find(Q, Tree)))).

find_with_any_test() ->
    xl_application:start(xl_stdlib),
    Tree = xl_uxekdtree:new(?POINTS_FOR_SMALL_TREE_WITH_UNDEFS),
    Q = {1, b},
    ?assertEquals([b, ub1, ub2], lists:sort(element(2, xl_uxekdtree:find(Q, Tree)))).

find_with_x_test() ->
    xl_application:start(xl_stdlib),
    Tree = xl_uxekdtree:new(?POINTS_FOR_SMALL_TREE_WITH_EXCLUDES),
    Q = {1, b},
    ?assertEquals([b, xb2], lists:sort(element(2, xl_uxekdtree:find(Q, Tree)))).

find_with_variance_test() ->
    xl_application:start(xl_stdlib),
    Tree = xl_uxekdtree:new(?POINTS_FOR_SMALL_TREE_WITH_EXCLUDES),
    Q = {1, [b, c]},
    ?assertEquals([b, c, xb2], lists:sort(element(2, xl_uxekdtree:find(Q, Tree)))).

find_test_() ->
    xl_application:start(xl_stdlib),
    {timeout, 2000, fun() ->
        {Tree, Points} = prepare_space(10, 1000),
        Queries = make_random_queries(Points, 10),
        ExpectedResuts = extract_results(Points, Queries, fun erlang:'=='/2),
        xl_lists:times(fun() ->
            {ok, Q} = xl_lists:random(Queries),
            Expected = xl_lists:kvfind(Q, ExpectedResuts),
            ?assertEquals(Expected, xl_uxekdtree:find(Q, Tree)),
            xl_eunit:performance(uxekdtree_find, fun() ->
                xl_uxekdtree:find(Q, Tree)
            end, 1000)
        end, 10)
    end}.

find_undefined_test_() ->
    xl_application:start(xl_stdlib),
    {timeout, 2000, fun() ->
        {Tree, Points} = prepare_space(10, 1000),
        Queries = [undefine(Q, 8, 10) || Q <- make_random_queries(Points, 10)],
        ExpectedResults = extract_results(Points, Queries, fun undefined_match/2),
        xl_eunit:format("muplitple results expected: ~p~n", [
            length(lists:filter(fun({_, Values}) -> length(Values) > 1 end, ExpectedResults))
        ]),
        xl_lists:times(fun() ->
            {ok, Q} = xl_lists:random(Queries),
            {ok, Expected} = xl_lists:kvfind(Q, ExpectedResults),
            ?assertEquals(Expected, lists:sort(element(2, xl_uxekdtree:find(Q, Tree)))),
            xl_eunit:performance(xl_uxekdtree_find_undef_q, fun() ->
                xl_uxekdtree:find(Q, Tree)
            end, 1000)
        end, 10)
    end}.

find_with_any_test_() ->
    xl_application:start(xl_stdlib),
    {timeout, 2000, fun() ->
        {Tree, Points} = prepare_space(10, 1000, 5),
        Queries = make_random_queries(Points, 10),
        ExpectedResults = extract_results(Points, Queries, fun undefined_match/2),
        xl_eunit:format("muplitple results expected: ~p~n", [
            length(lists:filter(fun({_, Values}) -> length(Values) > 1 end, ExpectedResults))
        ]),
        xl_lists:times(fun() ->
            {ok, Q} = xl_lists:random(Queries),
            {ok, Expected} = xl_lists:kvfind(Q, ExpectedResults),
            ?assertEquals(Expected, lists:sort(element(2, xl_uxekdtree:find(Q, Tree)))),
            xl_eunit:performance(xl_uxekdtree_find_w_any, fun() ->
                xl_uxekdtree:find(Q, Tree)
            end, 1000)
        end, 10)
    end}.

undefined_match(P, Q) when is_tuple(P) -> undefined_match(tuple_to_list(P), tuple_to_list(Q));
undefined_match([], []) -> true;
undefined_match([undefined | PT], [_ | QT]) -> undefined_match(PT, QT);
undefined_match([_ | PT], [undefined | QT]) -> undefined_match(PT, QT);
undefined_match([H | PT], [H | QT]) -> undefined_match(PT, QT);
undefined_match(_, _) -> false.

generate_points(PlaneSizes, Count, Undefined) ->
    Planes = lists:map(fun(Size) -> lists:seq(1, Size) end, PlaneSizes),
    [undefine(list_to_tuple([element(2, xl_lists:random(P)) || P <- Planes] ++ [V]), Undefined, length(Planes)) || V <- lists:seq(1, Count)].

generate_planes(Variabililty, Count) ->
    [element(2, xl_lists:random(Variabililty)) || _ <- lists:seq(1, Count)].

prepare_space(Dimensions, TotalPoints) -> prepare_space(Dimensions, TotalPoints, 0).
prepare_space(Dimensions, TotalPoints, Undefined) ->
    Planes = generate_planes([100, 50, 10], Dimensions),
    Points = generate_points(Planes, TotalPoints, Undefined),
    {Time, Tree} = timer:tc(xl_uxekdtree, new, [Points]),
    xl_eunit:format("planes: ~p:~p\t\t\tpoints: ~p\t'any' positions: ~p\tsize: ~p\tdepth: ~p\tconstruction time: ~p mcs~n", [
        length(Planes),
        list_to_tuple(Planes),
        TotalPoints,
        Undefined,
        xl_uxekdtree:size(Tree),
        xl_uxekdtree:depth(Tree),
        Time
    ]),
    {Tree, Points}.


point_to_query(P) -> list_to_tuple(element(1, lists:split(tuple_size(P) - 1, tuple_to_list(P)))).

make_random_queries(Points, Count) ->
    [point_to_query(element(2, xl_lists:random(Points))) || _ <- lists:seq(1, Count)].

extract_results(Points, Queries, Match) ->
    [{Q, lists:sort(lists:map(
        fun(T) -> element(tuple_size(T), T) end,
        lists:filter(fun(P) -> Match(point_to_query(P), Q) end, Points)
    ))} || Q <- Queries].


sorter_test() ->
    Sorter = xl_uxekdtree:get_sorter(1, fun xl_uxekdtree:default_comparator/3),
    Expected = [
        {undefined, b, ub1}, {undefined, b, ub2}, {1, c, c}, {1, b, b}, {2, undefined, uc},
        {2, c, c}, {2, c, c}, {3, a, a}, {3, a, a}, {3, a, a}
    ],
    xl_lists:times(fun() ->
        ?assertEquals(Expected, lists:sort(Sorter, ?POINTS_FOR_SMALL_TREE_WITH_UNDEFS))
    end, 10).

undefine(Tuple, Count, Length) ->
    {Positions, _} = lists:foldl(fun(_, {R, L}) ->
        {ok, E} = xl_lists:random(L),
        {[E | R], lists:delete(E, L)}
    end, {[], lists:seq(1, Length)}, lists:seq(1, Count)),
    lists:foldl(fun(P, Q) ->
        setelement(P, Q, undefined)
    end, Tuple, Positions).

%%      basic: points: 145250	size: 52344	depth: 26	construction time: 1628181 mcs
real_space_test_() ->
    {timeout, 200, fun() ->
        {ok, Data} = xl_file:read_file(xl_eunit:resource(?MODULE, "space")),
        Points = binary_to_term(Data),
        {Time, Tree} = timer:tc(xl_uxekdtree, new, [Points]),
        xl_eunit:format("points: ~p\tsize: ~p\tdepth: ~p\tconstruction time: ~p mcs~n", [
            length(Points),
            xl_uxekdtree:size(Tree),
            xl_uxekdtree:depth(Tree),
            Time
        ]),
        Qs = [{false, <<"CC">>, <<"IAB-19">>, undefined, 1, undefined, site, mediba,
            'Mon', 1, '320x50', <<"Samsung">>, <<"Galaxy S">>, <<"Android 4.0">>},
            {false, <<"US">>, <<"IAB-19">>, undefined, 1, undefined, site, nexage,
                'Mon', 1, '320x50', <<"Samsung">>, <<"Galaxy S">>, <<"Android 4.0">>},
            {false, <<"US">>, [<<"IAB-19">>, <<"IAB-20">>], undefined, 1, undefined, site, nexage,
                'Mon', 1, '320x50', <<"Samsung">>, <<"Galaxy S">>, <<"Android 4.0">>},
            {false, <<"GB">>, <<"IAB-19">>, undefined, 1, undefined, site, adiquity,
                'Mon', 1, '300x250', <<"Samsung">>, <<"Galaxy S">>, <<"Android 4.0">>},
            {false, <<"GB">>, <<"IAB-19">>, undefined, 1, undefined, site, adiquity,
                'Mon', 1, ['300x250', '320x50'], <<"Samsung">>, <<"Galaxy S">>, <<"Android 4.0">>}
        ],
        lists:foreach(fun(Q) ->
            xl_eunit:format("~p: ~p~n", [Q, length(element(2, xl_uxekdtree:find(Q, Tree)))]),
            xl_eunit:performance(xl_uxekdtree_real_space_find, fun() ->
                xl_uxekdtree:find(Q, Tree)
            end, 1000)
        end, Qs)
    end}.
