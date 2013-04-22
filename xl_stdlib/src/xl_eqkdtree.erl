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
-module(xl_eqkdtree).
-author("volodymyr.kyrychenko@strikead.com").

-compile({no_auto_import, [size/1]}).

%% API
-export([new/2, size/1, depth/1, new/1, find/2]).

-export_type([tree/0, tree_node/0, leaf/0, point/0, comparator/0, find_point/0]).

-type(comparator() :: fun((Plane :: pos_integer(), term(), term()) -> eq | lt | gt)).
-type(point() :: tuple()).
-type(find_point() :: tuple()).
-type(leaf() :: option_m:monad([point()])).
-type(tree_node() :: {term(), pos_integer(), tree_node(), tree_node(), tree_node()} | leaf()).
-type(tree() :: {module(), tree_node(), comparator()}).


-spec(new([point()]) -> tree()).
new(Points) -> new(Points, []).

-spec(new([point()], xl_lists:kvlist_at()) -> tree()).
new(Points, Options) -> {?MODULE, new_tree(Points, get_comparator(Options), 1, planes(Points)), Options}.

new_tree(Points, _Compare, _PlanePos, []) -> {ok, lists:map(fun(P) -> element(tuple_size(P), P) end, Points)};
new_tree([], _Compare, _PlanePos, _Planes) -> undefined;
new_tree(Points, Compare, PlanePos, Planes) when PlanePos > length(Planes) -> new_tree(Points, Compare, 1, Planes);
new_tree(Points, Compare, PlanePos, Planes) ->
    Plane = lists:nth(PlanePos, Planes),
%%     @todo distribution based sort?
    Sorted = lists:keysort(Plane, Points),
    Median = lists:nth(round(length(Sorted) / 2), Sorted),
    MedianValue = element(Plane, Median),
%%     @todo use more efficient split with no reverse because order is not important
%%     @todo use comparator for include/exclude/any criteria
    {Less, Rest} = lists:splitwith(fun(X) -> Compare(Plane, element(Plane, X), MedianValue) == lt end, Sorted),
    {Equal, Greater} = lists:splitwith(fun(X) -> Compare(Plane, element(Plane, X), MedianValue) == eq end, Rest),
    {
        MedianValue,
        Plane,
        new_tree(Less, Compare, PlanePos + 1, Planes),
        new_tree(Equal, Compare, PlanePos + 1, lists:delete(Plane, Planes)),
        new_tree(Greater, Compare, PlanePos + 1, Planes)
    }.

planes([]) -> [];
planes([H | _]) -> lists:seq(1, tuple_size(H) - 1).

- spec(size(tree()) -> pos_integer()).
size({?MODULE, Node, _Compare}) -> size(Node, 0).

-spec(size(tree_node(), pos_integer()) -> pos_integer()).
size(undefined, Count) -> Count;
size({ok, _}, Count) -> Count;
size({_, _, L, E, R}, Count) ->
    LNodes = size(L, Count + 1),
    ENodes = size(E, LNodes),
    size(R, ENodes).

-spec(depth(tree()) -> pos_integer()).
depth({?MODULE, Node, _Compare}) -> depth(Node, 0).

-spec(depth(tree_node(), pos_integer()) -> pos_integer()).
depth(undefined, Depth) -> Depth;
depth({ok, _}, Depth) -> Depth;
depth({_, _, L, E, R}, Depth) -> lists:max([depth(L, Depth + 1), depth(E, Depth + 1), depth(R, Depth + 1)]).

-spec(find(find_point(), tree()) -> option_m:monad([term()])).
find(Query, {?MODULE, Node, Options}) -> find(Query, Node, get_comparator(Options)).

find(_Query, undefined, _Compare) -> undefined;
find(_Query, Ok = {ok, _}, _Compare) -> Ok;
find(Query, {Value, Plane, L, E, R}, Compare) ->
    case element(Plane, Query) of
        undefined ->
            case monad:flatten(option_m, [find(Query, L, Compare), find(Query, E, Compare), find(Query, R, Compare)]) of
                [] -> undefined;
                X -> {ok, X}
            end;
        X ->
            case Compare(Plane, X, Value) of
                eq -> find(Query, E, Compare);
                lt -> find(Query, L, Compare);
                gt -> find(Query, R, Compare)
            end
    end.

get_comparator(Options) ->
    xl_lists:kvfind(compare, Options, fun(_Plane, Q, V) -> xl_lists:compare(Q, V) end).