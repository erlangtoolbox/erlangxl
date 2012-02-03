-module(strikead_list).

-export([find/2, first/1]).

find(_Pred, []) -> not_found;
find(Pred, [H|T]) ->
    case Pred(H) of
        true -> {ok, H};
        _ -> find(Pred, T)
    end.

first([]) -> empty;
first([H|_]) -> {ok, H}.
