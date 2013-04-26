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
-module(xl_uxekdtree).
-author("volodymyr.kyrychenko@strikead.com").

-compile({no_auto_import, [size/1]}).

%% API
-export([new/2, size/1, depth/1, new/1, find/2, get_sorter/2, default_comparator/3]).

-export_type([tree/0, tree_node/0, leaf/0, point/0, comparator/0, find_point/0]).

-type(comparator() :: fun((Plane :: pos_integer(), term(), term()) -> eq | lt | gt)).
-type(point() :: tuple()).
-type(find_point() :: tuple()).
-type(leaf() :: option_m:monad([point()])).
-type(tree_node() :: {term(), pos_integer(), Undefined :: tree_node(), Less :: tree_node(), Equal :: tree_node(), Greater :: tree_node(), Excluded :: tree_node()} | leaf()).
-type(tree() :: {module(), tree_node(), comparator()}).


-spec(new([point()]) -> tree()).
new(Points) -> new(Points, []).

-spec(new([point()], xl_lists:kvlist_at()) -> tree()).
new(Points, Options) -> {?MODULE, new_tree(Points, get_comparator(Options), 1, planes(Points)), Options}.

new_tree([], _Compare, _PlanePos, _Planes) -> [];
new_tree(Points, _Compare, _PlanePos, []) -> lists:map(fun(P) -> element(tuple_size(P), P) end, Points);
new_tree(Points, Compare, PlanePos, Planes) when PlanePos > length(Planes) -> new_tree(Points, Compare, 1, Planes);
new_tree(Points, Compare, PlanePos, Planes) ->
    Plane = lists:nth(PlanePos, Planes),
    {Undefs, Defs} = xl_lists:keypartition(Plane, undefined, Points),
    {MedianValue, Less, Equal, Greater, Excluded} = case Defs of
        [] -> {[], [], [], [], []};
        _ ->
            Sorted = lists:sort(get_sorter(Plane, Compare), Defs),
            Median = lists:nth(round(length(Sorted) / 2), Sorted),
            MV = value(element(Plane, Median)),
            {L, Rest} = xl_lists:fastsplitwith(fun(X) -> Compare(Plane, value(element(Plane, X)), MV) == lt end, Sorted),
            {Eq, G} = xl_lists:fastsplitwith(fun(X) -> Compare(Plane, value(element(Plane, X)), MV) == eq end, Rest),
            {E, X} = lists:partition(fun(X) -> case element(Plane, X) of {_, _} -> false; _ -> true end end, Eq),
            {MV, L, E, G, X}
    end,
    {
        MedianValue,
        Plane,
        new_tree(Undefs, Compare, PlanePos + 1, lists:delete(Plane, Planes)),
        new_tree(Less, Compare, PlanePos + 1, Planes),
        new_tree(Equal, Compare, PlanePos + 1, lists:delete(Plane, Planes)),
        new_tree(Greater, Compare, PlanePos + 1, Planes),
        new_tree(Excluded, Compare, PlanePos + 1, lists:delete(Plane, Planes))
    }.

planes([]) -> [];
planes([H | _]) -> lists:seq(1, tuple_size(H) - 1).

- spec(size(tree()) -> pos_integer()).
size({?MODULE, Node, _Compare}) -> size(Node, 0).

-spec(size(tree_node(), pos_integer()) -> pos_integer()).
size([], Count) -> Count;
size(L, Count) when is_list(L) -> Count;
size({_, _, U, L, E, R, X}, Count) ->
    UNodes = size(U, Count + 1),
    LNodes = size(L, UNodes),
    ENodes = size(E, LNodes),
    RNodes = size(R, ENodes),
    size(X, RNodes).

-spec(depth(tree()) -> pos_integer()).
depth({?MODULE, Node, _Compare}) -> depth(Node, 0).

-spec(depth(tree_node(), pos_integer()) -> pos_integer()).
depth([], Depth) -> Depth;
depth(L, Depth) when is_list(L) -> Depth;
depth({_, _, U, L, E, R, X}, Depth) ->
    lists:max([
        depth(U, Depth + 1),
        depth(L, Depth + 1),
        depth(E, Depth + 1),
        depth(R, Depth + 1),
        depth(X, Depth + 1)
    ]).

-spec(find(find_point(), tree()) -> option_m:monad([term()])).
find(Query, {?MODULE, Node, Options}) ->
    case find(Query, Node, get_comparator(Options), []) of
        [] -> undefined;
        R -> {ok, R}
    end.

find(_Query, [], _Compare, Acc) -> Acc;
find(_Query, L, _Compare, Acc) when is_list(L) -> L ++ Acc;
find(Query, {_Value, Plane, U, L, E, R, X}, Compare, Acc) when element(Plane, Query) == undefined ->
    UAcc = find(Query, U, Compare, Acc),
    LAcc = find(Query, L, Compare, UAcc),
    EAcc = find(Query, E, Compare, LAcc),
    RAcc = find(Query, R, Compare, EAcc),
    find(Query, X, Compare, RAcc);
find(Query, {Value, Plane, U, L, E, R, X}, Compare, Acc) ->
    UAcc = find(Query, U, Compare, Acc),
    case Compare(Plane, element(Plane, Query), Value) of
        eq -> find(Query, E, Compare, UAcc);
        lt ->
            LAcc = find(Query, L, Compare, UAcc),
            find(Query, X, Compare, LAcc);
        gt ->
            RAcc = find(Query, R, Compare, UAcc),
            find(Query, X, Compare, RAcc)
    end.

get_comparator(Options) ->
    xl_lists:kvfind(compare, Options, fun default_comparator/3).

%% prebuild sorters
get_sorter(Plane, Compare) ->
    fun(X, Y) ->
        case Compare(Plane, value(element(Plane, X)), value(element(Plane, Y))) of
            gt -> false;
            _ -> true
        end
    end.

value({x, V}) -> V;
value(V) -> V.

default_comparator(_Plane, undefined, undefined) -> eq;
default_comparator(_Plane, undefined, _) -> lt;
default_comparator(_Plane, _, undefined) -> gt;
default_comparator(_Plane, X, X) -> eq;
default_comparator(_Plane, X, Y) when X > Y -> gt;
default_comparator(_Plane, _, _) -> lt.
