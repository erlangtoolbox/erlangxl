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
-module(xl_tdb_index).
-author("volodymyr.kyrychenko@strikead.com").

-compile({no_auto_import, [size/1]}).

%% API
-export([size/1, depth/1, new/1, find/2, new/2]).

-export_type([tree/0, tree_node/0, leaf/0, point/0, query_point/0]).

-type(point() :: tuple()).
-type(query_point() :: tuple()).
-type(leaf() :: option_m:monad([point()])).
-type(tree_node() :: {
    term(), pos_integer(),
    Undefined :: tree_node(),
    Less :: tree_node(),
    Equal :: tree_node(),
    Greater :: tree_node(),
    Excluded :: [{xl_bloom:ref(), tree_node()}],
    Included :: [{xl_bloom:ref(), tree_node()}]
} | leaf()).
-type(tree() :: tree_node()).

-define(is_exclude(X), is_tuple(X) andalso element(1, X) == x).
-define(is_include(X), is_tuple(X) andalso element(1, X) == i).

-spec(new([point()]) -> tree()).
new(Points) -> new(Points, [{expansion_limit, 10}]).

-spec(new([point()], [{expansion_limit, pos_integer()}]) -> tree()).
new(Points, [{expansion_limit, Limit}]) ->
    XPoints = xl_tdb_index_lib:expand(Points, Limit),
    new_tree(XPoints, 1, xl_tdb_index_lib:planes(XPoints)).

new_tree([], _PlanePos, _Planes) -> [];
new_tree(Points, _PlanePos, []) -> xl_lists:set(lists:map(fun(P) -> element(tuple_size(P), P) end, Points));
new_tree(Points, PlanePos, Planes) when PlanePos > length(Planes) -> new_tree(Points, 1, Planes);
new_tree(Points, PlanePos, Planes) ->
    Plane = lists:nth(PlanePos, Planes),
    {Undefs, Defs} = xl_lists:keypartition(Plane, undefined, Points),
    {Excluded, Other} = lists:partition(fun(XE) -> ?is_exclude(element(Plane, XE)) end, Defs),
    {Included, Normal} = lists:partition(fun(XE) -> ?is_include(element(Plane, XE)) end, Other),
    {MedianValue, Less, Equal, Greater} = case Normal of
        [] -> {undefined, [], [], []};
        _ ->
            Sorted = lists:sort(xl_tdb_index_lib:sorter(Plane), Normal),
            Median = lists:nth(round(length(Sorted) / 2), Sorted),
            MV = element(Plane, Median),
            {L, Rest} = xl_lists:fastsplitwith(fun(XE) ->
                xl_tdb_index_lib:compare(element(Plane, XE), MV) == lt end, Sorted),
            {E, G} = xl_lists:fastsplitwith(fun(XE) ->
                xl_tdb_index_lib:compare(element(Plane, XE), MV) == eq end, Rest),
            {MV, L, E, G}
    end,
    PlanesWOOne = lists:delete(Plane, Planes),
    {
        MedianValue,
        Plane,
        new_tree(Undefs, PlanePos, PlanesWOOne),
        new_tree(Less, PlanePos + 1, Planes),
        new_tree(Equal, PlanePos, PlanesWOOne),
        new_tree(Greater, PlanePos + 1, Planes),
        new_list_tree(Excluded, Plane, PlanePos, PlanesWOOne, false),
        new_list_tree(Included, Plane, PlanePos, PlanesWOOne, true)
    }.

new_list_tree(Points, Plane, PlanePos, Planes, KeepValues) ->
    Dict = xl_lists:transform(dict, fun(Point) -> {element(2, element(Plane, Point)), Point} end, Points),
    case KeepValues of
        true ->
            [{element(2, xl_bloom:new(Values)), Values, new_tree(Pts, PlanePos, Planes)}
                || {Values, Pts} <- dict:to_list(Dict)];
        false ->
            [{element(2, xl_bloom:new(Values)), new_tree(Pts, PlanePos, Planes)}
                || {Values, Pts} <- dict:to_list(Dict)]
    end.

-spec(size(tree()) -> pos_integer()).
size(Node) -> size(Node, 0).

-spec(size(tree_node(), non_neg_integer()) -> non_neg_integer()).
size(L, Count) when is_list(L) -> Count;
size({_, _, U, L, E, R, XL, IL}, Count) ->
    UNodes = size(U, Count + 1),
    LNodes = size(L, UNodes),
    ENodes = size(E, LNodes),
    RNodes = size(R, ENodes),
    XNodes = list_size(XL, RNodes),
    list_size(IL, XNodes).

list_size(L, Nodes) ->
    lists:foldl(fun
        ({_, T}, S) -> size(T, S);
        ({_, _, T}, S) -> size(T, S)
    end, Nodes, L).

-spec(depth(tree()) -> non_neg_integer()).
depth(Node) -> depth(Node, 0).

-spec(depth(tree_node(), non_neg_integer()) -> non_neg_integer()).
depth(L, Depth) when is_list(L) -> Depth;
depth({_, _, U, L, E, R, XL, IL}, Depth) ->
    lists:max([
        depth(U, Depth + 1),
        depth(L, Depth + 1),
        depth(E, Depth + 1),
        depth(R, Depth + 1),
        list_depth(XL, Depth + 1),
        list_depth(IL, Depth + 1)
    ]).
list_depth(L, Depth) ->
    case length(L) of
        0 -> 0;
        _ ->
            lists:max(lists:map(fun
                ({_, T}) -> depth(T, Depth);
                ({_, _, T}) -> depth(T, Depth)
            end, L))
    end.

%% todo presort lists in query and replace partition with efficient split
-spec(find(query_point(), tree()) -> option_m:monad([term()])).
find(Query, Node) ->
    case find(Query, Node, []) of
        [] -> undefined;
        R -> {ok, R}
    end.
find(_Query, L, Acc) when is_list(L) -> L ++ Acc;
find(Query, {_Value, Plane, U, L, E, R, XL, IL}, Acc) when element(Plane, Query) == undefined ->
    UAcc = find(Query, U, Acc),
    LAcc = find(Query, L, UAcc),
    EAcc = find(Query, E, LAcc),
    RAcc = find(Query, R, EAcc),
%%      RAcc;
    XAcc = lists:foldl(fun({_, T}, A) -> find(Query, T, A) end, RAcc, XL),
    lists:foldl(fun({_, _, T}, A) -> find(Query, T, A) end, XAcc, IL);
find(Query, {Value, Plane, U, L, E, R, XL, IL}, Acc) when is_list(element(Plane, Query)) ->
    UAcc = find(Query, U, Acc),
    QL = element(Plane, Query),
    {QLess, QRest} = lists:partition(fun(QV) -> xl_tdb_index_lib:compare(QV, Value) == lt end, QL),
    {QEq, QGreater} = lists:partition(fun(QV) -> xl_tdb_index_lib:compare(QV, Value) == eq end, QRest),
    EAcc = case QEq of
        [_ | _] -> find(Query, E, UAcc);
        _ -> UAcc
    end,
    XAcc = lists:foldl(fun({Bloom, T}, FoldAcc) ->
        case lists:all(fun(QValue) ->
            not xl_bloom:contains(QValue, Bloom)
        end, QL) of
            true -> find(Query, T, FoldAcc);
            false -> FoldAcc
        end
    end, EAcc, XL),
    IAcc = lists:foldl(fun({Bloom, Values, T}, FoldAcc) ->
        case lists:any(fun(QValue) ->
            xl_bloom:contains(QValue, Bloom) andalso lists:member(QValue, Values)
        end, QL) of
            true -> find(Query, T, FoldAcc);
            false -> FoldAcc
        end
    end, XAcc, IL),
%%     IAcc = EAcc,
    LAcc = case QLess of
        [] -> IAcc;
        [LV] -> find(setelement(Plane, Query, LV), L, IAcc);
        _ -> find(setelement(Plane, Query, QLess), L, IAcc)
    end,
    case QGreater of
        [] -> LAcc;
        [GV] -> find(setelement(Plane, Query, GV), R, LAcc);
        _ -> find(setelement(Plane, Query, QGreater), R, LAcc)
    end;
find(Query, {Value, Plane, U, L, E, R, XL, IL}, Acc) ->
    UAcc = find(Query, U, Acc),
    QValue = element(Plane, Query),
    XAcc = lists:foldl(fun({Bloom, T}, FoldAcc) ->
        case xl_bloom:contains(QValue, Bloom) of
            false -> find(Query, T, FoldAcc);
            true -> FoldAcc
        end
    end, UAcc, XL),
    IAcc = lists:foldl(fun({Bloom, Values, T}, FoldAcc) ->
        case xl_bloom:contains(QValue, Bloom) andalso lists:member(QValue, Values) of
            true -> find(Query, T, FoldAcc);
            false -> FoldAcc
        end
    end, XAcc, IL),
%%     IAcc = UAcc,
    case xl_tdb_index_lib:compare(QValue, Value) of
        eq -> find(Query, E, IAcc);
        lt -> find(Query, L, IAcc);
        gt -> find(Query, R, IAcc)
    end.
