-module(strikead_json_rt).

-export([subst/3, from_dict/2, tuple2json/1]).

subst(X, X, Y) -> Y;
subst(X, _, _) -> X.

from_dict(_Field, undefined) -> undefined;
from_dict(Field, Dict) when is_tuple(Dict), element(1, Dict) == dict ->
    case dict:find(atom_to_list(Field), Dict) of
        error -> undefined;
        {ok, null} -> undefined;
        {ok, X} -> X
    end;
from_dict(_Field, Dict) -> throw({badarg, Dict}).

tuple2json(undefined) -> null;
tuple2json(T) when is_tuple(T) -> "{" ++ list2json(tuple_to_list(T)) ++ "}".

list2json([H]) when is_tuple(H), size(H) == 2 -> "\"" ++ atom_to_list(element(1, H)) ++ "\": " ++ value2json(element(2,H));
list2json([H|T]) when is_tuple(H), size(H) == 2 -> "\"" ++ atom_to_list(element(1, H)) ++ "\": " ++ value2json(element(2,H)) ++ ", " ++ list2json(T);
list2json(X) -> throw({badarg, {X, "must be 2 place tuples"}}).

value2json(undefined) -> "null";
value2json(true) -> "true";
value2json(false) -> "false";
value2json(X) when is_tuple(X) -> tuple2json(X);
value2json(X) when is_integer(X); is_float(X) -> lists:flatten(io_lib:format("~p", [X]));
value2json([]) -> "\"\"";
value2json(L = [X|_]) when is_tuple(X) -> "["++ list2json(L) ++"]";
value2json(L) -> lists:flatten(io_lib:format("~p", [L])).
