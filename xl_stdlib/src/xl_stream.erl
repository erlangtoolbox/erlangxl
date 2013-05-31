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
-module(xl_stream).

-export([stream/2, map/2, foreach/2, seq/2, foldl/3, filter/2, to_list/1, ifoldl/3, to_stream/1, to_pair/1, mapfind/2,
    empty/0, to_random_stream/1, keyfilter/3, eforeach/2, to_rpc_stream/1, to_rpc_stream/2, matchfilter/2, concat/1, flatmap/2, listn/2, mapfilter/2]).

-type(stream(A) :: {?MODULE, fun(() -> [A | stream(A)])}).
-export_type([stream/1]).

stream(Context, Next) ->
    {?MODULE, fun() ->
        case Next(Context) of
            empty -> [];
            {R, C} -> [R | stream(C, Next)]
        end
    end}.

map(F, S) ->
    {?MODULE, fun() ->
        case to_pair(S) of
            [] -> [];
            [H | T] -> [F(H) | map(F, T)]
        end
    end}.

foreach(F, S) ->
    case to_pair(S) of
        [] -> ok;
        [H | T] -> F(H), foreach(F, T)
    end.

eforeach(F, S) ->
    case to_pair(S) of
        [] -> ok;
        [H | T] ->
            case F(H) of
                ok -> eforeach(F, T);
                {ok, _} -> eforeach(F, T);
                E -> E
            end
    end.

seq(From, To) ->
    stream(From, fun(X) ->
        if
            X =< To -> {X, X + 1};
            true -> empty
        end
    end).

foldl(F, Acc0, S) ->
    case to_pair(S) of
        [] -> Acc0;
        [H | T] -> foldl(F, F(H, Acc0), T)
    end.


ifoldl(F, Acc0, S) -> ifoldl(F, Acc0, 1, S).

ifoldl(F, Acc0, Index, S) ->
    case to_pair(S) of
        [] -> Acc0;
        [H | T] -> ifoldl(F, F(H, Acc0, Index), Index + 1, T)
    end.


filter(P, S) -> {?MODULE, fun() -> filter_next(P, S) end}.

filter_next(P, S) ->
    case to_pair(S) of
        [] -> [];
        [H | T] ->
            case P(H) of
                true -> [H | filter(P, T)];
                _ -> filter_next(P, T)
            end
    end.

-spec(mapfilter(xl_lists:mapping_predicate(term(), term()), stream(term())) -> stream(term())).
mapfilter(P, S) -> {?MODULE, fun() -> mapfilter_next(P, S) end}.

mapfilter_next(P, S) ->
    case to_pair(S) of
        [] -> [];
        [H | T] ->
            case P(H) of
                {ok, V} -> [V | mapfilter(P, T)];
                undefined -> mapfilter_next(P, T)
            end
    end.

to_list(S) -> lists:reverse(foldl(fun(V, L) -> [V | L] end, [], S)).

to_pair({?MODULE, S}) -> S().

to_stream(L) when is_list(L) ->
    stream(L, fun
        ([]) -> empty;
        ([H | T]) -> {H, T}
    end).

-spec mapfind/2 :: (fun((any()) -> option_m:monad(any())), stream(any())) -> option_m:monad(any()).
mapfind(F, S) ->
    case to_pair(S) of
        [] -> undefined;
        [H | T] ->
            case F(H) of
                undefined -> mapfind(F, T);
                R -> R
            end
    end.

empty() -> to_stream([]).

to_random_stream([]) -> empty();
to_random_stream(L) when is_list(L) ->
    Split = lists:split(xl_random:uniform(length(L)), L),
    stream(Split, fun
        ({[], []}) -> empty;
        ({[H | T], []}) -> {H, {T, []}};
        ({HL, [H | T]}) -> {H, {HL, T}}
    end).

keyfilter(Keys, KeyPos, S) -> {?MODULE, fun() -> keyfilter_next(Keys, KeyPos, S) end}.

keyfilter_next([], _KeyPos, _S) -> [];
keyfilter_next(Keys = [K | KT], KeyPos, S) ->
    case to_pair(S) of
        [] -> [];
        [H | T] when K == element(KeyPos, H) -> [H | keyfilter(KT, KeyPos, T)];
        [H | T] when K > element(KeyPos, H) -> keyfilter_next(Keys, KeyPos, T);
        _ -> keyfilter_next(KT, KeyPos, S)
    end.

to_rpc_stream(S) -> to_rpc_stream(node(), S).
to_rpc_stream(Node, S) ->
    stream(S, fun(Stream) ->
        case xl_rpc:call(Node, xl_stream, to_pair, [Stream]) of
            {ok, []} -> empty;
            {ok, [H | T]} -> {{ok, H}, T};
            E -> {E, []}
        end
    end).

listn(N, S) -> {?MODULE, fun() -> listn_next(N, [], S) end}.

listn_next(N, Acc, S) when length(Acc) == N -> [lists:reverse(Acc) | listn(N, S)];
listn_next(N, Acc, S) ->
    case to_pair(S) of
        [] when length(Acc) == 0 -> [];
        [] -> [lists:reverse(Acc) | empty()];
        [H | T] -> listn_next(N, [H | Acc], T)
    end.



matchfilter(_F, [X]) -> map(fun(E) -> [E] end, to_stream(X));
matchfilter(F, Lists) -> {?MODULE, fun() -> matchfilter_next(F, Lists) end}.

matchfilter_next(_F, []) -> [];
matchfilter_next(_F, [[] | _]) -> [];
matchfilter_next(F, [[H1 | T1] | TL]) ->
    case shift(F, H1, TL, {[], []}) of
        nomatch -> [];
        {next, Tails} -> matchfilter_next(F, [T1 | Tails]);
        {Values, Tails} -> [[H1 | Values] | matchfilter(F, [T1 | Tails])]
    end.

shift(_F, _K, [], Acc) -> Acc;
shift(_F, _K, [[] | _TL], _Acc) -> nomatch;
shift(F, K, L = [[H | T] | TL], Acc = {Values, Tails}) ->
    case F(K, H) of
        eq -> shift(F, K, TL, {[H | Values], [T | Tails]});
        gt -> shift(F, K, [T | TL], Acc);
        _ -> {next, Tails ++ L}
    end.

flatmap(F, S) -> {?MODULE, fun() -> flatmap_next(F, S) end}.
flatmap_next(F, S) ->
    case to_pair(S) of
        [] -> [];
        [H | T] ->
            case to_pair(F(H)) of
                [] -> flatmap_next(F, T);
                [HR | TR] -> [HR | concat([TR, flatmap(F, T)])]
            end
    end.

concat(Streams) -> stream(Streams, fun concat_next/1).

concat_next([]) -> empty;
concat_next([S | T]) ->
    case to_pair(S) of
        [] -> concat_next(T);
        [H | ST] -> {H, [ST | T]}
    end.
