-module(strikead_maybe_m).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1, just/1]).

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


return(X) -> {ok, X}.
fail(_) -> false.

just({ok, X}) -> X;
just(nothing) -> nothing.
