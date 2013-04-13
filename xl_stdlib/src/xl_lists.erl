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
-module(xl_lists).

-export([find/2, first/1, emap/2, eforeach/2, mapfilter/2, index/2, split/2, keypsort/3,
    sublistmatch/2, substitute/3, keyfind/3, keyfind/4, keyreplace/3, kvfind/2,
    kvfind/3, keyreplace_or_add/3, eflatten/1, insert_before/3, random/1,
    count_unique/1, keyincrement/3, split_by/2, efoldl/3, substitute/2, imap/2, intersect/2,
    mapfind/2, set/1, union/2, count/2, times/2, etimes/2, transform/3, seq/4, matchfilter/2,
    value_comparator/2, key_comparator/2, zip_with_index/1, nth/2, keymerge/4, shuffle/1]).

-type(kvlist(A, B) :: [{A, B}]).
-type(kvlist_at() :: kvlist(atom(), atom() | binary() | string() | integer() | float())).
-export_types([kvlist/2, kvlist_at/0]).

-spec(find(fun((term()) -> boolean()), [term()]) -> option_m:monad(term())).
find(_Pred, []) -> undefined;
find(Pred, [H | T]) ->
    case Pred(H) of
        true -> {ok, H};
        _ -> find(Pred, T)
    end.

-spec(mapfind(fun((term()) -> option_m:monad(term())), [term()]) -> option_m:monad(term())).
mapfind(_Pred, []) -> undefined;
mapfind(Pred, [H | T]) ->
    case Pred(H) of
        undefined -> mapfind(Pred, T);
        X -> X
    end.

-spec(first([term()]) -> option_m:monad(term())).
first([]) -> undefined;
first([H | _]) -> {ok, H}.

-spec(emap(fun((term()) -> error_m:monad(term())), [term()]) -> error_m:monad([term()])).
emap(F, List) -> emap(F, [], List).

-spec(emap(fun((term()) -> error_m:monad(term())), [term()], [term()]) -> error_m:monad([term()])).
emap(_F, Acc, []) -> {ok, lists:reverse(Acc)};
emap(F, Acc, [H | T]) ->
    case F(H) of
        {ok, R} -> emap(F, [R | Acc], T);
        X -> X
    end.

-spec(eforeach(fun((any()) -> error_m:monad(any())), []) -> error_m:monad(ok)).
eforeach(_F, []) -> ok;
eforeach(F, [H | T]) ->
    case F(H) of
        ok -> eforeach(F, T);
        {ok, _} -> eforeach(F, T);
        E -> E
    end.

-spec(efoldl(fun((term(), term())-> error_m:monad(term())), term(), [term()]) -> error_m:monad(term())).
efoldl(_F, Acc, []) -> {ok, Acc};
efoldl(F, Acc, [H | T]) ->
    case F(H, Acc) of
        {ok, NewAcc} -> efoldl(F, NewAcc, T);
        X -> X
    end.


-spec(mapfilter(fun((term()) -> false | term()), [term()]) -> [term()]).
mapfilter(F, L) -> mapfilter([], F, L).

-spec(mapfilter([term()], fun((term()) -> option_m:monad(term())), [term()]) -> [term()]).
mapfilter(Acc, _F, []) -> lists:reverse(Acc);
mapfilter(Acc, F, [H | T]) ->
    case F(H) of
        undefined -> mapfilter(Acc, F, T);
        {ok, X} -> mapfilter([X | Acc], F, T)
    end.

-spec(keypsort([term()], integer(), kvlist(term(), term())) -> [{term(), term()}]).
keypsort(Keys, N, L) ->
    C = fun(A, B) ->
        case {index(element(N, A), Keys), index(element(N, B), Keys)} of
            {undefined, _} -> true;
            {_, undefined} -> false;
            {{ok, I1}, {ok, I2}} -> I1 =< I2
        end
    end,
    lists:sort(C, L).

-spec(index(term(), [term()]) -> option_m:monad(integer())).
index(X, L) -> index(X, 1, L).

-spec(index(term(), integer(), [term()]) -> option_m:monad(integer())).
index(_X, _I, []) -> undefined;
index(X, I, [X | _]) -> {ok, I};
index(X, I, [_ | T]) -> index(X, I + 1, T).

-spec(sublistmatch(kvlist_at(), kvlist_at()) -> boolean()).
sublistmatch(Pattern, Map) ->
    lists:all(fun({Pk, Pv}) ->
        case lists:keyfind(Pk, 1, Map) of
            {Pk, V} when Pv == empty -> V == [];
            {Pk, V} when Pv == not_empty -> V /= [];
            {Pk, Pv} -> true;
            {Pk, V} when is_list(V) ->
                re:run(V, Pv, [anchored, {capture, none}]) == match;
            _ -> false
        end
    end, Pattern).

-spec(substitute([term()], kvlist_at()) -> [term()]).
substitute(Pattern, Map) -> substitute(Pattern, Map, fun xl_string:substitute/2).

-spec(substitute([term()], kvlist_at(), fun((string(), kvlist_at()) -> string())) -> [term()]).
substitute(Pattern, Map, StringHandler) ->
    lists:map(fun(X) ->
        case lists:keyfind(X, 1, Map) of
            {X, V} -> V;
            _ when is_list(X) -> StringHandler(X, Map);
            _ -> X
        end
    end, Pattern).

-spec(keyfind(term(), pos_integer(), [tuple()], tuple()) -> tuple()).
keyfind(Key, N, List, Default) ->
    case keyfind(Key, N, List) of
        {ok, X} -> X;
        undefined -> Default
    end.

-spec(keyfind(term(), pos_integer(), [tuple()]) -> option_m:monad(tuple())).
keyfind(Key, N, List) ->
    case lists:keyfind(Key, N, List) of
        false -> undefined;
        X -> {ok, X}
    end.

-spec(kvfind(term(), kvlist(any(), any()), any()) -> any()).
kvfind(Key, List, Default) ->
    case kvfind(Key, List) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

-spec(kvfind(term(), kvlist(any(), any())) -> option_m:monad(any())).
kvfind(Key, List) ->
    case keyfind(Key, 1, List) of
        {ok, {_, Value}} -> {ok, Value};
        X -> X
    end.

-spec(keyreplace_or_add(pos_integer(), [tuple()], tuple() | [tuple()]) -> [tuple()]).
keyreplace_or_add(N, List, List2) when is_list(List2) ->
    lists:foldl(fun(T, Acc) -> keyreplace_or_add(N, Acc, T) end, List, List2);
keyreplace_or_add(N, List, Tuple) when is_tuple(Tuple) ->
    case lists:keymember(element(N, Tuple), N, List) of
        true -> lists:keyreplace(element(N, Tuple), N, List, Tuple);
        false -> [Tuple | List]
    end.

-spec(keyreplace(pos_integer(), [tuple()], [tuple()]) -> [tuple()]).
keyreplace(_N, List, []) -> List;
keyreplace(N, List, [R | ReplList]) ->
    keyreplace(N, lists:keyreplace(element(N, R), N, List, R), ReplList).

-spec(split(pos_integer(), [term()]) -> {[term()], [term()]}).
split(Pos, List) when length(List) > Pos -> lists:split(Pos, List);
split(_, List) -> {List, []}.

-spec(eflatten(error_m:monad([term()])) -> error_m:monad([term()])).
eflatten({ok, List}) -> {ok, lists:flatten(List)};
eflatten(E) -> E.


-spec(insert_before(any(), any(), list()) -> list()).
insert_before(BeforeElem, Elem, List) ->
    {Head, Tail} = lists:splitwith(fun(E) ->
        case E of
            BeforeElem ->
                false;
            _ ->
                true
        end
    end, List),
    Head ++ [Elem] ++ Tail.

-spec(random([term()]) -> option_m:monad(term())).
random(List) ->
    case length(List) of
        0 -> undefined;
        L -> {ok, lists:nth(xl_random:uniform(L), List)}
    end.

-spec(count_unique([term()]) -> [{term(), integer()}]).
count_unique(List) -> count_unique(List, []).
count_unique([], Counter) -> Counter;
count_unique([H | T], Counter) -> count_unique(T, keyincrement(H, 1, Counter)).

-spec(keyincrement(term(), integer(), [{term(), integer()}]) -> [{term(), integer()}]).
keyincrement(Key, Inc, List) ->
    case kvfind(Key, List) of
        {ok, Count} -> keyreplace(1, List, [{Key, Count + Inc}]);
        undefined -> [{Key, Inc} | List]
    end.

-spec(split_by([term()], term()) -> [[term()]]).
split_by([], _Sep) -> [[]];
split_by(List, Sep) ->
    {H, T} = lists:splitwith(fun(X) -> X =/= Sep end, List),
    case T of
        [Sep | Rest] -> [H | split_by(Rest, Sep)];
        [] -> [H]
    end.

-spec(imap(fun((term(), pos_integer()) -> term()), [term()]) -> [term()]).
imap(F, List) -> imap(F, [], 1, List).
imap(_F, Acc, _Index, []) -> lists:reverse(Acc);
imap(F, Acc, Index, [H | T]) -> imap(F, [F(H, Index) | Acc], Index + 1, T).

intersect(List1, List2) ->
    L1 = set(List1),
    L2 = set(List2),
    lists:filter(fun(E) -> lists:member(E, L2) end, L1).

union(List1, List2) -> set(List1 ++ List2).

set(List) -> lists:reverse(set(lists:sort(List), [])).

set([], Acc) -> Acc;
set([H], Acc) -> [H | Acc];
set([H, H | T], Acc) -> set([H | T], Acc);
set([X, Y | T], Acc) -> set([Y | T], [X | Acc]).

count(Predicate, List) -> count(Predicate, List, 0).
count(_Predicate, [], Acc) -> Acc;
count(Predicate, [H | T], Acc) ->
    case Predicate(H) of
        true -> count(Predicate, T, Acc + 1);
        _ -> count(Predicate, T, Acc)
    end.

times(Fun, Count) when is_function(Fun, 1) -> lists:foreach(Fun, lists:seq(1, Count));
times(_Fun, 0) -> ok;
times(Fun, Count) -> Fun(), times(Fun, Count - 1).

etimes(Fun, Count) -> eforeach(Fun, lists:seq(1, Count)).

%% todo make it parse_trasform over [||]
-spec(transform(atom(), fun((term())-> term()), [term()]) -> term()).
transform(TargetStruct, NodeFun, List) ->
    lists:foldl(fun(X, S) -> transform_add(TargetStruct, NodeFun(X), S) end, transform_init(TargetStruct), List).

transform_init(gb_tree) -> gb_trees:empty().

transform_add(gb_tree, {Key, Value}, Tree) -> gb_trees:insert(Key, Value, Tree).

seq(Start, Stop, Step, F) when Start =< Stop -> [F(Start) | seq(Start + Step, Stop, Step, F)];
seq(_Start, _Stop, _Step, _F) -> [].


matchfilter(_F, [X]) -> lists:map(fun(E) -> [E] end, X);
matchfilter(F, Lists) -> matchfilter_next(F, Lists).

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

value_comparator(X, X) -> eq;
value_comparator(X, Y) when X > Y -> gt;
value_comparator(_, _) -> lt.

key_comparator({X, _}, {X, _}) -> eq;
key_comparator({X, _}, {Y, _}) when X > Y -> gt;
key_comparator(_, _) -> lt.

zip_with_index(List) -> imap(fun(T, I) -> {T, I} end, List).

nth(N, List) ->
    try
        {ok, lists:nth(N, List)}
    catch
        _:_ -> undefined
    end.

keymerge(Fun, Pos, List1, List2) ->
    lists:map(fun(V1) ->
        case keyfind(element(Pos, V1), Pos, List2) of
            {ok, V2} -> Fun(V1, V2);
            undefined -> V1
        end
    end, List1) ++ lists:filter(fun(V) ->
        not lists:keymember(element(Pos, V), Pos, List1)
    end, List2).


-spec(shuffle([term()]) -> [term()]).
shuffle(List) -> shuffle(xl_random:uniform(length(List)) - 1, List, []).

shuffle(0, L, R) -> L ++ R;
shuffle(N, [H | T], R) -> shuffle(N - 1, T, [H | R]).
