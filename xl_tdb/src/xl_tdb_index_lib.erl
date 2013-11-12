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
-export([expand/2, planes/1, sorter/1, compare/2, contains/2, estimate_expansion/2]).

-define(is_mexclude(X), is_tuple(X) andalso element(1, X) == x andalso is_list(element(2, X))).
-define(is_exclude(X), is_tuple(X) andalso element(1, X) == x).

-spec(expand([tuple()], pos_integer()) -> [tuple()]).
expand(Points, ExpansionLimit) ->
    lists:flatmap(fun(Point) ->
        expand_point(Point, tuple_size(Point) - 1, ExpansionLimit)
    end, Points).

expand_point(Point, 0, _ExpansionLimit) -> [Point];
expand_point(Point, N, ExpansionLimit) when is_list(element(N, Point)) ->
    L = element(N, Point),
    case L of
        [] -> expand_point(setelement(N, Point, undefined), N - 1, ExpansionLimit);
        _ when length(L) > ExpansionLimit -> expand_point(setelement(N, Point, {i, L}), N - 1, ExpansionLimit);
        _ -> lists:flatmap(fun(V) -> expand_point(setelement(N, Point, V), N - 1, ExpansionLimit) end, L)
    end;
expand_point(Point, N, ExpansionLimit) when ?is_mexclude(element(N, Point)) ->
    L = element(N, Point),
    case L of
        {x, []} -> expand_point(setelement(N, Point, undefined), N - 1, ExpansionLimit);
        _ -> expand_point(Point, N - 1, ExpansionLimit)
    end;
expand_point(Point, N, ExpansionLimit) when ?is_exclude(element(N, Point)) ->
    {x, X} = element(N, Point),
    expand_point(setelement(N, Point, {x, [X]}), N - 1, ExpansionLimit);
expand_point(Point, N, ExpansionLimit) -> expand_point(Point, N - 1, ExpansionLimit).


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

%% todo refactor using early exit if all planes are present
stat(Points, Size) ->
    lists:foldl(fun(P, Mask) ->
        lists:foldl(fun(Index, IMask) ->
            case element(Index, P) of
                undefined -> IMask;
                _ -> IMask bor (1 bsl (Index - 1))
            end
        end, Mask, lists:seq(1, Size))
    end, 0, Points).


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
