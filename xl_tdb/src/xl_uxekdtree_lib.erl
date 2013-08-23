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
-module(xl_uxekdtree_lib).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([expand/1, planes/1, sorter/1, compare/2, node_value/1]).

-define(is_mexclude(X), is_tuple(X) andalso element(1, X) == x andalso is_list(element(2, X))).
-define(is_exclude(X), is_tuple(X) andalso element(1, X) == x).

-spec(expand([tuple()]) -> [tuple()]).
expand(Points) -> lists:flatmap(fun(Point) -> expand_point(Point, tuple_size(Point) - 1) end, Points).

expand_point(Point, 0) -> [Point];
expand_point(Point, N) when is_list(element(N, Point)) ->
    L = element(N, Point),
    lists:flatmap(fun(V) ->
        expand_point(setelement(N, Point, V), N - 1)
    end, L);
expand_point(Point, N) when ?is_mexclude(element(N, Point)) ->
    {x, XL} = element(N, Point),
    lists:flatmap(fun(XV) ->
        expand_point(setelement(N, Point, {x, XV, lists:delete(XV, XL)}), N - 1)
    end, XL);
expand_point(Point, N) when ?is_exclude(element(N, Point)) ->
    {x, X} = element(N, Point),
    expand_point(setelement(N, Point, {x, X, []}), N - 1);
expand_point(Point, N) -> expand_point(Point, N - 1).



-spec(planes([tuple()]) -> [pos_integer()]).
planes([]) -> [];
planes(Points = [H | _]) ->
    Mask = stat(Points, tuple_size(H) - 1),
    Planes = lists:filter(fun(Index) ->
        Bit = (1 bsl (Index - 1)),
        Bit band Mask == Bit
    end, lists:seq(1, tuple_size(H) - 1)),
%%     xl_eunit:format("planes ~.2B, ~p~n", [Mask, Planes]),
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
        case compare(node_value(element(Plane, X)), node_value(element(Plane, Y))) of
            gt -> false;
            _ -> true
        end
    end.

node_value({x, V, _}) -> V;
node_value({x, V}) -> V;
node_value(V) -> V.

compare(undefined, undefined) -> eq;
compare(undefined, _) -> lt;
compare(_, undefined) -> gt;
compare(X, X) -> eq;
compare(X, Y) when X > Y -> gt;
compare(_, _) -> lt.




