%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is Erlando.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2011 VMware, Inc.  All rights reserved.
%%

%% List Monad. Mainly just for fun! As normal, this is depth first.

-module(list_m).

-behaviour(monad).
-behaviour(monad_plus).

-export(['>>='/2, return/1, fail/1]).

-export([mzero/0, mplus/2]).
-export_type([monad/1]).

-type(monad(A) :: [A]).

%% Note that using a list comprehension is (obviously) cheating, but
%% it's easier to read. The "real" implementation is also included for
%% completeness.

-spec('>>='(monad(A), fun ((A) -> monad(B))) -> monad(B)).
'>>='(X, Fun) -> lists:append([Fun(E) || E <- X]).
%%               lists:foldr(fun (E, Acc) -> Fun(E) ++ Acc end, [], X).

-spec(return(A) -> monad(A)).
return(X) -> [X].

-spec(fail(any()) -> monad(_A)).
fail(_X)  -> [].

-spec(mzero() -> monad(_A)).
mzero() -> [].

-spec(mplus(monad(A), monad(A)) -> monad(A)).
mplus(X, Y) -> X ++ Y.
