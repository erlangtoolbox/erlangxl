-module(strikead_json_rt).

-export([subst/3, from_dict/2]).

subst(X, X, Y) -> Y;
subst(X, _, _) -> X.

from_dict(Field, Dict) ->
    case dict:find(atom_to_list(Field), Dict) of
        error -> undefined;
        {ok, null} -> undefined;
        {ok, X} -> X
    end.
