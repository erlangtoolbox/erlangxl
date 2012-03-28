-module(strikead_lists).

-export([find/2, first/1, emap/2, mapfilter/2]).

find(_Pred, []) -> nothing;
find(Pred, [H|T]) ->
    case Pred(H) of
        true -> {ok, H};
        _ -> find(Pred, T)
    end.

first([]) -> nothing;
first([H|_]) -> {ok, H}.

emap(F, List) -> emap(F, [], List).

emap(_F, Acc, []) -> {ok, lists:reverse(Acc)};
emap(F, Acc, [H|T]) ->
    case F(H) of
        {ok, R} -> emap(F, [R|Acc], T);
        X -> X
    end.

-spec mapfilter(fun((term()) -> false | term()), [term()]) -> [term()].
mapfilter(F, L) -> mapfilter([], F, L).

-spec mapfilter([term()], fun((term()) -> nothing | {just, term()}), [term()]) -> [term()].
mapfilter(Acc, _F, []) -> lists:reverse(Acc);
mapfilter(Acc, F, [H | T]) ->
	case F(H) of
		nothing -> mapfilter(Acc, F, T);
		{ok, X} -> mapfilter([X | Acc], F, T)
	end.


