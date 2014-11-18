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

-module(monad_plus).

-compile({parse_transform, do}).

-export([guard/2, guard/3, msum/2, mfilter/3]).
-export_type([monad/1]).

-type(monad(_A) :: any()).

-callback(mzero() -> monad(_A)).
-callback(mplus(monad(A), monad(A)) -> monad(A)).

-spec(guard(atom(), boolean()) -> monad(_A)).
guard(Monad, true) -> Monad:return(ok);
guard(Monad, false) -> Monad:fail("guard failed").

-spec(guard(atom(), boolean(), any()) -> monad(_A)).
guard(Monad, true, _) -> Monad:return(ok);
guard(Monad, false, Failure) -> Monad:fail(Failure).

-spec(msum/2 :: (atom(), [monad(A)]) -> monad(A)).
msum(Monad, List) ->
    lists:foldr(fun Monad:mplus/2, Monad:mzero(), List).

-spec(mfilter/3 :: (atom(), fun((A) -> boolean()), monad(A)) -> monad(A)).
mfilter(Monad, Pred, X) ->
    do([Monad || A <- X,
        case Pred(A) of
            true -> return(A);
            false -> Monad:mzero()
        end]).
