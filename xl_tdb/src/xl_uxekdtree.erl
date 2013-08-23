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
%% -on_load(init/0).

%% API
-export([size/1, depth/1, new/1, find/2, dump/1, new/2]).

-export_type([tree/0, tree_node/0, leaf/0, point/0, find_point/0]).

-type(point() :: tuple()).
-type(find_point() :: tuple()).
-type(leaf() :: option_m:monad([point()])).
-type(tree_node() :: {term(), pos_integer(), Undefined :: tree_node(), Less :: tree_node(), Equal :: tree_node(), Greater :: tree_node(), Excluded :: tree_node()} | leaf()).
-type(tree() :: {?MODULE, tree_node()} | xl_ref:ref()).

-define(is_exclude(X), is_tuple(X) andalso element(1, X) == x).

%% init() -> erlang:load_nif(xl_lang:find_nif(xl_tdb, ?MODULE), 0).

-spec(new([point()]) -> tree()).
new(Points) -> new(Points, [shared]).

-spec(new([point()], xl_lists:kvlist_at()) -> tree()).
new(Points, [local]) -> {?MODULE, new_tree(xl_uxekdtree_lib:expand(Points), 1, xl_uxekdtree_lib:planes(Points))};
new(Points, [shared]) -> xl_ref:new(new(Points, [local])).

new_tree([], _PlanePos, _Planes) -> [];
new_tree(Points, _PlanePos, []) -> lists:map(fun(P) -> element(tuple_size(P), P) end, Points);
new_tree(Points, PlanePos, Planes) when PlanePos > length(Planes) -> new_tree(Points, 1, Planes);
new_tree(Points, PlanePos, Planes) ->
    Plane = lists:nth(PlanePos, Planes),
    {Undefs, Defs} = xl_lists:keypartition(Plane, undefined, Points),
    {MedianValue, Less, Equal, Greater, Excluded} = case Defs of
        [] -> {undefined, [], [], [], []};
        _ ->
            Sorted = lists:sort(xl_uxekdtree_lib:sorter(Plane), Defs),
            Median = lists:nth(round(length(Sorted) / 2), Sorted),
            MV = xl_uxekdtree_lib:node_value(element(Plane, Median)),
            {L, Rest} = xl_lists:fastsplitwith(fun(X) -> xl_uxekdtree_lib:compare(xl_uxekdtree_lib:node_value(element(Plane, X)), MV) == lt end, Sorted),
            {Eq, G} = xl_lists:fastsplitwith(fun(X) -> xl_uxekdtree_lib:compare(xl_uxekdtree_lib:node_value(element(Plane, X)), MV) == eq end, Rest),
            {X, E} = lists:partition(fun(X) -> ?is_exclude(element(Plane, X)) end, Eq),
            {MV, L, E, G, X}
    end,
    xl_eunit:format("X:~p~n", [Excluded]),
    PlanesWOOne = lists:delete(Plane, Planes),
    {
        MedianValue,
        Plane,
        new_tree(Undefs, PlanePos, PlanesWOOne),
        new_tree(Less, PlanePos + 1, Planes),
        new_tree(Equal, PlanePos, PlanesWOOne),
        new_tree(Greater, PlanePos + 1, Planes),
        new_exclude_tree(Excluded, Plane, PlanePos, PlanesWOOne)
    }.

new_exclude_tree(Excluded, Plane, PlanePos, Planes) ->
    Dict = xl_lists:transform(dict, fun(Point) -> {element(3, element(Plane, Point)), Point} end, Excluded),
    [{XCheck, new_tree(Pts, PlanePos, Planes)} || {XCheck, Pts} <- dict:to_list(Dict)].

-spec(size(tree()) -> pos_integer()).
size({?MODULE, Node}) -> size(Node, 0);
size(Ref) -> size(xl_ref:value(Ref)).

-spec(size(tree_node(), non_neg_integer()) -> non_neg_integer()).
size(L, Count) when is_list(L) -> Count;
size({_, _, U, L, E, R, X}, Count) ->
    UNodes = size(U, Count + 1),
    LNodes = size(L, UNodes),
    ENodes = size(E, LNodes),
    RNodes = size(R, ENodes),
    size(X, RNodes).

-spec(depth(tree()) -> non_neg_integer()).
depth({?MODULE, Node}) -> depth(Node, 0);
depth(Ref) -> depth(xl_ref:value(Ref)).

-spec(depth(tree_node(), non_neg_integer()) -> non_neg_integer()).
depth(L, Depth) when is_list(L) -> Depth;
depth({_, _, U, L, E, R, X}, Depth) ->
    lists:max([
        depth(U, Depth + 1),
        depth(L, Depth + 1),
        depth(E, Depth + 1),
        depth(R, Depth + 1),
        depth(X, Depth + 1)
    ]).
%% todo presort lists in query and replace partition with efficient split
-spec(find(find_point(), xl_ref:ref()) -> option_m:monad([term()])).
find(Query, {?MODULE, Node}) ->
    case find(Query, Node, []) of
        [] -> undefined;
        R -> {ok, R}
    end;
find(Query, Ref) -> find(Query, xl_ref:value(Ref)).

find(_Query, L, Acc) when is_list(L) -> L ++ Acc;
find(Query, {_Value, Plane, U, L, E, R, X}, Acc) when element(Plane, Query) == undefined ->
    UAcc = find(Query, U, Acc),
    LAcc = find(Query, L, UAcc),
    EAcc = find(Query, E, LAcc),
    RAcc = find(Query, R, EAcc),
    find(Query, X, RAcc);
find(Query, {Value, Plane, U, L, E, R, X}, Acc) when is_list(element(Plane, Query)) ->
    UAcc = find(Query, U, Acc),
    QL = element(Plane, Query),
    {QLess, QRest} = lists:partition(fun(QV) -> xl_uxekdtree_lib:compare(QV, Value) == lt end, QL),
    {QEq, QGreater} = lists:partition(fun(QV) -> xl_uxekdtree_lib:compare(QV, Value) == eq end, QRest),
    EAcc = case QEq of
        [_ | _] -> find(Query, E, UAcc);
        _ -> UAcc
    end,
    XAcc = case {QLess, QGreater} of
        {[], []} -> EAcc;
        _ -> find(Query, X, EAcc)
    end,
    LAcc = case QLess of
        [] -> XAcc;
        [LV] -> find(setelement(Plane, Query, LV), L, XAcc);
        _ -> find(setelement(Plane, Query, QLess), L, XAcc)
    end,
    case QGreater of
        [] -> LAcc;
        [GV] -> find(setelement(Plane, Query, GV), R, LAcc);
        _ -> find(setelement(Plane, Query, QGreater), R, LAcc)
    end;
find(Query, {Value, Plane, U, L, E, R, X}, Acc) ->
    UAcc = find(Query, U, Acc),
    case xl_uxekdtree_lib:compare(element(Plane, Query), Value) of
        eq -> find(Query, E, UAcc);
        lt ->
            LAcc = find(Query, L, UAcc),
            find(Query, X, LAcc);
        gt ->
            RAcc = find(Query, R, UAcc),
            find(Query, X, RAcc)
    end.

-spec(dump(tree()) -> tree_node()).
dump(T = {?MODULE, _}) -> T;
dump(Ref) -> xl_ref:value(Ref).

