%% =============================================================================
%%  The MIT License (MIT)
%%
%%  Copyright (c) 2014 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy of
%%  this software and associated documentation files (the "Software"), to deal in
%%  the Software without restriction, including without limitation the rights to
%%  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
%%  the Software, and to permit persons to whom the Software is furnished to do so,
%%  subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in all
%%  copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
%%  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
%%  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
%%  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%%  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%% =============================================================================

-module(option_m).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-behaviour(monad).
-behaviour(monad_plus).

-export(['>>='/2, return/1, fail/1]).
-export([mzero/0, mplus/2]).
-export([get/2, to_error_m/2, to_list/1]).
-export_type([monad/1]).

-type(monad(A) :: {ok, A} | undefined).

-spec('>>='(monad(A), fun((A) -> monad(B))) -> monad(B)).
'>>='({ok, X}, Fun) -> Fun(X);
'>>='(undefined, _Fun) -> undefined.

-spec(return(A) -> monad(A)).
return(X) -> {ok, X}.

-spec(fail(any()) -> monad(_A)).
fail(_X) -> undefined.

-spec(mzero() -> monad(_A)).
mzero() -> undefined.

-spec(mplus(monad(A), monad(A)) -> monad(A)).
mplus(undefined, Y) -> Y;
mplus(X, _Y) -> X.

-spec(get(monad(A), A) -> A).
get(undefined, Default) -> Default;
get({ok, X}, _) -> X.

-spec(to_error_m(monad(A), any()) -> error_m:monad(A)).
to_error_m(undefined, Error) -> {error, Error};
to_error_m(X, _) -> X.

-spec(to_list(monad(A)) -> [A]).
to_list({ok, X}) -> [X];
to_list(undefined) -> [].
