-module(strikead_lists).

-export([find/2, first/1, emap/2, mapfilter/2]).

-spec find/2 :: (fun((term()) -> boolean()), [term()]) -> strikead_maybe_m:monad(term()).
find(_Pred, []) -> nothing;
find(Pred, [H|T]) ->
    case Pred(H) of
        true -> {ok, H};
        _ -> find(Pred, T)
    end.

-spec first/1 :: ([term()]) -> strikead_maybe_m:monad(term()).
first([]) -> nothing;
first([H|_]) -> {ok, H}.

-spec emap/2 :: (fun((term()) -> error_m:monad(term())), [term()]) -> error_m:monad([term()]).
emap(F, List) -> emap(F, [], List).

-spec emap/3 :: (fun((term()) -> error_m:monad(term())), [term()], [term()]) -> error_m:monad([term()]).
emap(_F, Acc, []) -> {ok, lists:reverse(Acc)};
emap(F, Acc, [H|T]) ->
    case F(H) of
        {ok, R} -> emap(F, [R|Acc], T);
        X -> X
    end.

-spec mapfilter/2 :: (fun((term()) -> false | term()), [term()]) -> [term()].
mapfilter(F, L) -> mapfilter([], F, L).

-spec mapfilter/3 :: ([term()], fun((term()) -> strikead_maybe_m:monad(term())), [term()]) -> [term()].
mapfilter(Acc, _F, []) -> lists:reverse(Acc);
mapfilter(Acc, F, [H | T]) ->
	case F(H) of
		nothing -> mapfilter(Acc, F, T);
		{ok, X} -> mapfilter([X | Acc], F, T)
	end.
