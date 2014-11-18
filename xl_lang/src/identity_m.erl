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

-module(identity_m).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).
-export_type([monad/1]).

-type(monad(A) :: A).

-spec('>>='(monad(A), fun((A) -> monad(B))) -> monad(B)).
'>>='(X, Fun) -> Fun(X).

-spec(return(A) -> monad(A)).
return(X) -> X.

-spec(fail(any()) -> monad(_A)).
fail(X) -> throw({error, X}).
