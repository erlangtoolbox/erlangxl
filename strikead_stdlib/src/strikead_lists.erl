-module(strikead_lists).

-export([find/2, first/1, emap/2, mapfilter/2, index/2, keypsort/3, sublistmatch/2]).

-spec find/2 :: (fun((term()) -> boolean()), [term()])
	-> strikead_maybe_m:monad(term()).
find(_Pred, []) -> nothing;
find(Pred, [H|T]) ->
	case Pred(H) of
		true -> {ok, H};
		_ -> find(Pred, T)
	end.

-spec first/1 :: ([term()]) -> strikead_maybe_m:monad(term()).
first([]) -> nothing;
first([H|_]) -> {ok, H}.

-spec emap/2 :: (fun((term()) -> error_m:monad(term())), [term()])
	-> error_m:monad([term()]).
emap(F, List) -> emap(F, [], List).

-spec emap/3 :: (fun((term()) -> error_m:monad(term())), [term()], [term()])
	-> error_m:monad([term()]).
emap(_F, Acc, []) -> {ok, lists:reverse(Acc)};
emap(F, Acc, [H|T]) ->
	case F(H) of
		{ok, R} -> emap(F, [R|Acc], T);
		X -> X
	end.

-spec mapfilter/2 :: (fun((term()) -> false | term()), [term()]) -> [term()].
mapfilter(F, L) -> mapfilter([], F, L).

-spec mapfilter/3 :: ([term()],
	fun((term()) -> strikead_maybe_m:monad(term())), [term()]) -> [term()].
mapfilter(Acc, _F, []) -> lists:reverse(Acc);
mapfilter(Acc, F, [H | T]) ->
	case F(H) of
		nothing -> mapfilter(Acc, F, T);
		{ok, X} -> mapfilter([X | Acc], F, T)
	end.

-spec keypsort/3 :: ([term()], integer(), [{term(), term()}])
	-> [{term(), term()}].
keypsort(Keys, N, L) ->
	C = fun(A, B) ->
		case {index(element(N, A), Keys), index(element(N, B), Keys)} of
			{nothing, _} -> true;
			{_, nothing} -> false;
			{{ok, I1}, {ok, I2}} -> I1 =< I2
		end
	end,
	lists:sort(C, L).

-spec index/2 :: (term(), [term()]) -> strikead_maybe_m:monad(integer()).
index(X, L) -> index(X, 1, L).

-spec index/3 :: (term(), integer(), [term()])
	-> strikead_maybe_m:monad(integer()).
index(_X, _I, []) -> nothing;
index(X, I, [X | _]) -> {ok, I};
index(X, I, [_ | T]) -> index(X, I + 1, T).

-spec sublistmatch/2 :: ([{atom(), term()}], [{atom(), term()}]) -> boolean().
sublistmatch(Pattern, List) ->
	lists:all(fun({Pk, Pv}) ->
		case lists:keyfind(Pk, 1, List) of
			{Pk, Pv} -> true;
			{Pk, V} when is_list(V) ->
				re:run(V, Pv, [anchored, {capture, none}]) == match;
			_ -> false
		end
	end, Pattern).
