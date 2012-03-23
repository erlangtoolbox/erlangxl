-module(strikead_list).

-export([find/2, first/1, emap/2]).

find(_Pred, []) -> not_found;
find(Pred, [H|T]) ->
    case Pred(H) of
        true -> {ok, H};
        _ -> find(Pred, T)
    end.

first([]) -> empty;
first([H|_]) -> {ok, H}.

emap(F, List) -> emap(F, [], List).

emap(_F, Acc, []) -> {ok, lists:reverse(Acc)};
emap(F, Acc, [H|T]) ->
    case F(H) of
        {ok, R} -> emap(F, [R|Acc], T);
        X -> X
    end.


