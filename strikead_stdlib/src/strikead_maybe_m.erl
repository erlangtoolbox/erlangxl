-module(strikead_maybe_m).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).
-export_type([monad/1]).

-type monad(A) :: {ok, A} | nothing.
-type adapted_monad(A) :: {ok, A} | {just, A} | nothing | not_found | error | false | A.

-spec '>>='/2 :: (adapted_monad(A), fun((A) -> monad(B))) -> monad(B).
'>>='({just, X}, F) -> F(X);
'>>='({ok, X}, F) -> F(X);
'>>='(nothing, _F) -> nothing;
'>>='(not_found, _F) -> nothing;
%dict
'>>='(error, _F) -> nothing;
%lists:keyfind
'>>='(false, _F) -> nothing;
%lists:keyfind
'>>='(X, F) -> F(X).


-spec return/1 :: (A) -> monad(A).
return(X) -> {ok, X}.
-spec fail/1 :: (any()) -> monad(_A).
fail(_) -> nothing.

