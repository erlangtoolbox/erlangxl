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
-module(xl_tdb_index_lib).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([expand/2, planes/1, sorter/1, compare/2, contains/2, estimate_expansion/2, ueipartition/2, splitwith/3, ixfilter/2]).

-define(is_exclude(X), is_tuple(X) andalso element(1, X) == x).
-define(is_mexclude(X), ?is_exclude(X) andalso is_list(element(2, X))).
-define(is_include(X), is_tuple(X) andalso element(1, X) == i).

-spec(expand([tuple()], pos_integer()) -> [tuple()]).
expand(Points, ExpansionLimit) ->
    lists:flatmap(fun(Point) ->
        TupleSize = tuple_size(Point),
        UpdatedPoint = setelement(TupleSize, Point, {[], element(TupleSize, Point)}),
        expand_point(UpdatedPoint, TupleSize - 1, TupleSize, ExpansionLimit)
    end, Points).

expand_point(Point, 0, _ValuePos, _ExpansionLimit) -> [Point];
expand_point(Point, N, ValuePos, ExpansionLimit) when is_list(element(N, Point)) ->
    L = element(N, Point),
    case L of
        [] -> expand_point(setelement(N, Point, undefined), N - 1, ValuePos, ExpansionLimit);
        _ when length(L) > ExpansionLimit ->
            {Rules, Value} = element(ValuePos, Point),
            Rule = case length(L) > 100 of
                true -> {ibloom, N, {xl_bloom:new(L), L}};
                false -> {i, N, L}
            end,
            NewPoint = setelement(ValuePos, Point, {[Rule | Rules], Value}),
            expand_point(setelement(N, NewPoint, undefined), N - 1, ValuePos, ExpansionLimit);
        _ ->
            lists:flatmap(fun(V) ->
                expand_point(setelement(N, Point, V), N - 1, ValuePos, ExpansionLimit)
            end, L)
    end;
expand_point(Point, N, ValuePos, ExpansionLimit) when ?is_mexclude(element(N, Point)) ->
    case (element(N, Point)) of
        {x, []} ->
            expand_point(setelement(N, Point, undefined), N - 1, ValuePos, ExpansionLimit);
        {x, L} ->
            {Rules, Value} = element(ValuePos, Point),
            Rule = case length(L) > 100 of
                true -> {xbloom, N, xl_bloom:new(L)};
                false -> {x, N, L}
            end,
            NewPoint = setelement(ValuePos, Point, {[Rule | Rules], Value}),
            expand_point(setelement(N, NewPoint, undefined), N - 1, ValuePos, ExpansionLimit)
    end;
expand_point(Point, N, ValuePos, ExpansionLimit) when ?is_exclude(element(N, Point)) ->
    {x, X} = element(N, Point),
    {Rules, Value} = element(ValuePos, Point),
    NewPoint = setelement(ValuePos, Point, {[{xitem, N, X} | Rules], Value}),
    expand_point(setelement(N, NewPoint, undefined), N - 1, ValuePos, ExpansionLimit);
expand_point(Point, N, ValuePos, ExpansionLimit) ->
    expand_point(Point, N - 1, ValuePos, ExpansionLimit).


-spec(estimate_expansion([tuple()], pos_integer()) -> non_neg_integer()).
estimate_expansion(Points, ExpansionLimit) ->
    lists:foldl(fun(Point, S) ->
        S + estimate_expansion_point(Point, tuple_size(Point) - 1, ExpansionLimit)
    end, 0, Points).

estimate_expansion_point(_Point, 0, _ExpansionLimit) -> 1;
estimate_expansion_point(Point, N, ExpansionLimit) when is_list(element(N, Point)) ->
    case length(element(N, Point)) of
        0 -> estimate_expansion_point(Point, N - 1, ExpansionLimit);
        X when X > ExpansionLimit -> estimate_expansion_point(Point, N - 1, ExpansionLimit);
        X -> X * estimate_expansion_point(Point, N - 1, ExpansionLimit)
    end;
estimate_expansion_point(Point, N, ExpansionLimit) -> estimate_expansion_point(Point, N - 1, ExpansionLimit).


-spec(planes([tuple()]) -> [pos_integer()]).
planes([]) -> [];
planes(Points = [H | _]) ->
    Mask = stat(Points, tuple_size(H) - 1),
    Planes = lists:filter(fun(Index) ->
        Bit = (1 bsl (Index - 1)),
        Bit band Mask == Bit
    end, lists:seq(1, tuple_size(H) - 1)),
    Planes.

stat(Points, Size) -> stat(Points, Size, 0).
stat([], _Size, Mask) -> Mask;
stat([H | T], Size, Mask) ->
    M = lists:foldl(fun(Index, IMask) ->
        case element(Index, H) of
            undefined -> IMask;
            _ -> IMask bor (1 bsl (Index - 1))
        end
    end, Mask, lists:seq(1, Size)),
    case trunc(math:pow(2, Size)) - 1 of
        M -> M;
        _ -> stat(T, Size, M)
    end.


%% prebuild sorters
sorter(Plane) ->
    fun(X, Y) ->
        case compare(element(Plane, X), element(Plane, Y)) of
            gt -> false;
            _ -> true
        end
    end.

compare(undefined, undefined) -> eq;
compare(undefined, _) -> lt;
compare(_, undefined) -> gt;
compare(X, X) -> eq;
compare(X, Y) when X > Y -> gt;
compare(_, _) -> lt.

-spec(contains(term(), [term()]) -> boolean()).
contains(K, L) when is_list(L) -> lists:member(K, L).

ueipartition(Plane, Points) -> ueipartition(Plane, Points, [], [], [], []).

ueipartition(_Plane, [], UAcc, XAcc, IAcc, NAcc) -> {UAcc, XAcc, IAcc, NAcc};
ueipartition(Plane, [H | T], UAcc, XAcc, IAcc, NAcc) when element(Plane, H) == undefined ->
    ueipartition(Plane, T, [H | UAcc], XAcc, IAcc, NAcc);
ueipartition(Plane, [H | T], UAcc, XAcc, IAcc, NAcc) when ?is_exclude(element(Plane, H)) ->
    ueipartition(Plane, T, UAcc, [H | XAcc], IAcc, NAcc);
ueipartition(Plane, [H | T], UAcc, XAcc, IAcc, NAcc) when ?is_include(element(Plane, H)) ->
    ueipartition(Plane, T, UAcc, XAcc, [H | IAcc], NAcc);
ueipartition(Plane, [H | T], UAcc, XAcc, IAcc, NAcc) ->
    ueipartition(Plane, T, UAcc, XAcc, IAcc, [H | NAcc]).

splitwith(Plane, Value, Points) -> splitwith(Plane, Value, Points, [], [], []).
splitwith(_Plane, Value, [], LAcc, EAcc, GAcc) -> {Value, LAcc, EAcc, GAcc};
splitwith(Plane, Value, [H | T], LAcc, EAcc, GAcc) ->
    case compare(element(Plane, H), Value) of
        lt -> splitwith(Plane, Value, T, [H | LAcc], EAcc, GAcc);
        eq -> splitwith(Plane, Value, T, LAcc, [H | EAcc], GAcc);
        gt -> splitwith(Plane, Value, T, LAcc, EAcc, [H | GAcc])
    end.

-spec(ixfilter(tuple(), xl_lists:mapping_predicate() | [{[{x|i, pos_integer(), [term()]}], term()}]) ->
    xl_lists:mapping_predicate()).
ixfilter(Q, F) when is_function(F, 1) ->
    fun({Filters, Value}) ->
        case accept(Filters, Q) of
            true -> F(Value);
            false -> undefined
        end
    end;
ixfilter(Q, Values) when is_list(Values) ->
    xl_lists:mapfilter(fun({Filters, Value}) ->
        case accept(Filters, Q) of
            true -> {ok, Value};
            false -> undefined
        end
    end, Values).

accept(Filters, Q) when is_list(Filters) ->
    lists:all(fun(Filter) -> accept(Filter, Q) end, Filters);

accept({xitem, Pos, Value}, Q) when is_list(element(Pos, Q)) ->
    not lists:any(fun(E) -> E == Value end, element(Pos, Q));
accept({xitem, Pos, Value}, Q) ->
    element(Pos, Q) /= Value;

accept({x, Pos, Values}, Q) when is_list(element(Pos, Q)) ->
    not lists:any(fun(E) -> lists:member(E, Values) end, element(Pos, Q));
accept({x, Pos, Values}, Q) ->
    not lists:member(element(Pos, Q), Values);

accept({xbloom, Pos, Bloom}, Q) when is_list(element(Pos, Q)) ->
    not lists:any(fun(E) -> xl_bloom:contains(E, Bloom) end, element(Pos, Q));
accept({xbloom, Pos, Bloom}, Q) ->
    not xl_bloom:contains(element(Pos, Q), Bloom);

accept({i, Pos, Values}, Q) when is_list(element(Pos, Q)) ->
    lists:any(fun(E) -> lists:member(E, Values) end, element(Pos, Q));
accept({i, Pos, Values}, Q) ->
    lists:member(element(Pos, Q), Values);

accept({ibloom, Pos, {Bloom, Values}}, Q) when is_list(element(Pos, Q)) ->
    lists:any(fun(E) -> xl_bloom:contains(E, Bloom) andalso lists:member(E, Values) end, element(Pos, Q));
accept({ibloom, Pos, {Bloom, Values}}, Q) ->
    Value = element(Pos, Q),
    xl_bloom:contains(Value, Bloom) andalso lists:member(Value, Values).
