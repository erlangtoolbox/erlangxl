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
-export([size/1, depth/1, new/1, find/2, sorter/1, compare/2, dump/1]).

-export_type([tree/0, tree_node/0, leaf/0, point/0, find_point/0]).

-type(point() :: tuple()).
-type(find_point() :: tuple()).
-type(leaf() :: option_m:monad([point()])).
-type(tree_node() :: {term(), pos_integer(), Undefined :: tree_node(), Less :: tree_node(), Equal :: tree_node(), Greater :: tree_node(), Excluded :: tree_node()} | leaf()).
-type(tree() :: {module(), tree_node()}).


-spec(new([point()]) -> xl_ref:ref()).
new(Points) -> xl_ref:new({?MODULE, new_tree(Points, 1, planes(Points))}).

new_tree([], _PlanePos, _Planes) -> [];
new_tree(Points, _PlanePos, []) -> lists:map(fun(P) -> element(tuple_size(P), P) end, Points);
new_tree(Points, PlanePos, Planes) when PlanePos > length(Planes) -> new_tree(Points, 1, Planes);
new_tree(Points, PlanePos, Planes) ->
    Plane = lists:nth(PlanePos, Planes),
    {Undefs, Defs} = xl_lists:keypartition(Plane, undefined, Points),
    {MedianValue, Less, Equal, Greater, Excluded} = case Defs of
        [] -> {[], [], [], [], []};
        _ ->
            Sorted = lists:sort(sorter(Plane), Defs),
            Median = lists:nth(round(length(Sorted) / 2), Sorted),
            MV = value(element(Plane, Median)),
            {L, Rest} = xl_lists:fastsplitwith(fun(X) -> compare(value(element(Plane, X)), MV) == lt end, Sorted),
            {Eq, G} = xl_lists:fastsplitwith(fun(X) -> compare(value(element(Plane, X)), MV) == eq end, Rest),
            {E, X} = lists:partition(fun(X) -> case element(Plane, X) of {_, _} -> false; _ -> true end end, Eq),
            {MV, L, E, G, X}
    end,
    {
        MedianValue,
        Plane,
        new_tree(Undefs, PlanePos + 1, lists:delete(Plane, Planes)),
        new_tree(Less, PlanePos + 1, Planes),
        new_tree(Equal, PlanePos + 1, lists:delete(Plane, Planes)),
        new_tree(Greater, PlanePos + 1, Planes),
        new_tree(Excluded, PlanePos + 1, lists:delete(Plane, Planes))
    }.

planes([]) -> [];
planes(Points = [H | _]) ->
    Mask = data_mask(Points, tuple_size(H) - 1),
    Planes = lists:filter(fun(Index) ->
        Bit = (1 bsl (Index - 1)),
        Bit band Mask == Bit
    end, lists:seq(1, tuple_size(H) - 1)),
%%     xl_eunit:format("planes ~.2B, ~p~n", [Mask, Planes]),
    Planes.

data_mask(Points, Size) ->
    lists:foldl(fun(P, Mask) ->
        lists:foldl(fun(Index, IMask) ->
            case element(Index, P) of
                undefined -> IMask;
                _ -> IMask bor (1 bsl (Index - 1))
            end
        end, Mask, lists:seq(1, Size))
    end, 0, Points).

-spec(size(xl_ref:ref()) -> pos_integer()).
size(Ref) ->
    {?MODULE, Node} = xl_ref:value(Ref),
    size(Node, 0).

-spec(size(tree_node(), non_neg_integer()) -> non_neg_integer()).
size([], Count) -> Count;
size(L, Count) when is_list(L) -> Count;
size({_, _, U, L, E, R, X}, Count) ->
    UNodes = size(U, Count + 1),
    LNodes = size(L, UNodes),
    ENodes = size(E, LNodes),
    RNodes = size(R, ENodes),
    size(X, RNodes).

-spec(depth(xl_ref:ref()) -> non_neg_integer()).
depth(Ref) ->
    {?MODULE, Node} = xl_ref:value(Ref),
    depth(Node, 0).

-spec(depth(tree_node(), non_neg_integer()) -> non_neg_integer()).
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

-spec(find(find_point(), xl_ref:ref()) -> option_m:monad([term()])).
find(Query, Ref) ->
    {?MODULE, Node} = xl_ref:value(Ref),
    case find(Query, Node, []) of
        [] -> undefined;
        R -> {ok, R}
    end.

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
    {QLess, QRest} = lists:partition(fun(QV) -> compare(QV, Value) == lt end, QL),
    {QEq, QGreater} = lists:partition(fun(QV) -> compare(QV, Value) == eq end, QRest),
    EAcc = case QEq of [_ | _] ->
        find(Query, E, UAcc);
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
    case compare(element(Plane, Query), Value) of
        eq -> find(Query, E, UAcc);
        lt ->
            LAcc = find(Query, L, UAcc),
            find(Query, X, LAcc);
        gt ->
            RAcc = find(Query, R, UAcc),
            find(Query, X, RAcc)
    end.

-spec(dump(xl_ref:ref()) -> tree()).
dump(Ref) -> xl_ref:value(Ref).

%% prebuild sorters
sorter(Plane) ->
    fun(X, Y) ->
        case compare(value(element(Plane, X)), value(element(Plane, Y))) of
            gt -> false;
            _ -> true
        end
    end.

value({x, V}) -> V;
value(V) -> V.

compare(undefined, undefined) -> eq;
compare(undefined, _) -> lt;
compare(_, undefined) -> gt;
compare(X, X) -> eq;
compare(X, Y) when X > Y -> gt;
compare(_, _) -> lt.

