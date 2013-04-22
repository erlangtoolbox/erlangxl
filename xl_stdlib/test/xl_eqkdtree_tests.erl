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
-module(xl_eqkdtree_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").
-include("xl_eunit.hrl").

generate_points(PlaneSizes, Count) ->
    Planes = lists:map(fun(Size) -> lists:seq(1, Size) end, PlaneSizes),
    [list_to_tuple([element(2, xl_lists:random(P)) || P <- Planes] ++ [V]) || V <- lists:seq(1, Count)].

generate_planes(Variabililty, Count) ->
    [element(2, xl_lists:random(Variabililty)) || _ <- lists:seq(1, Count)].

new_test() ->
    xl_application:start(xl_stdlib),
    Points = [{1, c, c}, {1, b, b}, {3, a, a}, {2, c, c}, {1, b, b}, {3, a, a}, {2, c, c}, {1, b, b}, {3, a, a}, {2, c, c}],
    Compare = fun(_Plane, X, Y) -> xl_lists:compare(X, Y) end,
    ExpectedTree = {xl_eqkdtree,
        {2, 1,
            {b, 2,
                undefined,
                {1, 1,
                    undefined,
                    {ok, [b, b, b]},
                    undefined
                },
                {1, 1,
                    undefined,
                    {c, 2,
                        undefined,
                        {ok, [c]},
                        undefined
                    },
                    undefined
                }
            },
            {c, 2,
                undefined,
                {ok, [c, c, c]},
                undefined
            },
            {a, 2,
                undefined,
                {3, 1,
                    undefined,
                    {ok, [a, a, a]},
                    undefined
                },
                undefined
            }
        }, [{compare, Compare}]},
    ?assertEquals(ExpectedTree, xl_eqkdtree:new(Points, [{compare, Compare}])).

new_performance_test_() ->
    xl_application:start(xl_stdlib),
    {timeout, 2000, fun() ->
        xl_eunit:performance(eqkdtree, fun() ->
            prepare_space(10, 1000)
        end, 5)
    end}.

find_test_() ->
    xl_application:start(xl_stdlib),
    {timeout, 2000, fun() ->
        {Tree, Points} = prepare_space(10, 1000),
        Queries = make_random_queries(Points, 10),
        ExpectedResuts = prepare_expected_results(Points, Queries, fun erlang:'=='/2),
        xl_lists:times(fun() ->
            {ok, Q} = xl_lists:random(Queries),
            Expected = xl_lists:kvfind(Q, ExpectedResuts),
            xl_eunit:performance(eqkdtree_find, fun() ->
                ?assertEquals(Expected, xl_eqkdtree:find(Q, Tree))
            end, 10000)
        end, 10)
    end}.

find_undefined_test_() ->
    xl_application:start(xl_stdlib),
    {timeout, 2000, fun() ->
        {Tree, Points} = prepare_space(10, 1000),
        Queries = [lists:foldl(fun(_, Q) ->
            setelement(xl_random:uniform(10), Q, undefined)
        end, Q0, lists:seq(1, 12)) || Q0 <- make_random_queries(Points, 10)],
        ExpectedResults = prepare_expected_results(Points, Queries, fun undefined_match/2),
        xl_eunit:format("muplitple results expected: ~p~n", [
            length(lists:filter(fun({_, Values}) -> length(Values) > 1 end, ExpectedResults))
        ]),
        xl_lists:times(fun() ->
            {ok, Q} = xl_lists:random(Queries),
            {ok, Expected} = xl_lists:kvfind(Q, ExpectedResults),
            xl_eunit:performance(eqkdtree_find, fun() ->
                ?assertEquals(Expected, lists:sort(element(2, xl_eqkdtree:find(Q, Tree))))
            end, 1000)
        end, 10)
    end}.

undefined_match(P, Q) when is_tuple(P) -> undefined_match(tuple_to_list(P), tuple_to_list(Q));
undefined_match([], []) -> true;
undefined_match([_ | PT], [undefined | QT]) -> undefined_match(PT, QT);
undefined_match([H | PT], [H | QT]) -> undefined_match(PT, QT);
undefined_match(_, _) -> false.



prepare_space(Dimensions, TotalPoints) ->
    Planes = generate_planes([100, 50, 10], Dimensions),
    Points = generate_points(Planes, TotalPoints),
    UniquePoints = xl_lists:count_unique(Points),
    {Time, Tree} = timer:tc(xl_eqkdtree, new, [Points]),
    xl_eunit:format("planes: ~p:~p\t\t\tpoints: ~p\tunique: ~p\tsize: ~p\tdepth: ~p\tconstruction time: ~p mcs~n", [
        length(Planes),
        list_to_tuple(Planes),
        TotalPoints,
        length(UniquePoints),
        xl_eqkdtree:size(Tree),
        xl_eqkdtree:depth(Tree),
        Time
    ]),
    {Tree, Points}.

point_to_query(P) -> list_to_tuple(element(1, lists:split(tuple_size(P) - 1, tuple_to_list(P)))).

make_random_queries(Points, Count) ->
    [point_to_query(P) || _ <- lists:seq(1, Count), P <- [element(2, xl_lists:random(Points))]].

prepare_expected_results(Points, Queries, Match) ->
    [{Q, lists:map(
        fun(T) -> element(tuple_size(T), T) end,
        lists:filter(fun(P) -> Match(point_to_query(P), Q) end, Points)
    )} || Q <- Queries].
