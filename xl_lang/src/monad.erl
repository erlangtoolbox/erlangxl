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
%% Copyright (c) 2011-2012 VMware, Inc.  All rights reserved.
%%

-module(monad).

-compile({parse_transform, do}).

-export([join/2, sequence/2, flatten/2]).
-export_type([monad/1]).

%% dream of higher kinded types
-type(monad(A) :: A).

-callback(return(A) -> monad(A)).
-callback(fail(any()) -> monad(_A)).
-callback('>>='(monad(A), fun((A) -> monad(B))) -> monad(B)).

-spec(join(atom(), monad(monad(A))) -> monad(A)).
join(Monad, X) -> do([Monad || Y <- X, Y]).

-spec(sequence(atom(), [monad(A)]) -> monad([A])).
sequence(Monad, Xs) -> sequence(Monad, Xs, []).

sequence(Monad, [], Acc) -> do([Monad || return(lists:reverse(Acc))]);
sequence(Monad, [X | Xs], Acc) -> do([Monad || E <- X, sequence(Monad, Xs, [E | Acc])]).

-spec(flatten(atom(), [monad(A)]) -> [A]).
flatten(Monad, List) -> lists:flatten([Monad:to_list(X) || X <- List]).
