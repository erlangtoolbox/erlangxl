%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(epath).
-author("Volodymyr Kyrychenko <volodymyr.kyrychenko@strikead.com>").

-compile({parse_transform, do}).

%% API
-export([select/2, select/3, update/3, update/4, concat/3, concat/4, eselect/3,
    eselect/4]).

-spec eselect/4 :: (string(), list(), tuple() | list(), term()) -> error_m:monad(term()).
eselect(EPath, Params, X, Error) ->
    eselect(xl_string:format(EPath, Params), X, Error).

-spec eselect/3 :: (string(), tuple() | list(), term()) -> error_m:monad(term()).
eselect(EPath, X, Error) ->
    do([error_m ||
        R <- select(EPath, X),
        option_m:to_error_m(R, Error)
    ]).

-spec select/3 :: (string(), list(), tuple() | list()) -> error_m:monad(option_m:monad(term())).
select(EPath, Params, X) -> select(xl_string:format(EPath, Params), X).

-spec select/2 :: (string(), tuple() | list()) -> error_m:monad(option_m:monad(term())).
select(EPath, X) ->
    do([error_m ||
        Tree <- parse(EPath),
        return(select_(Tree, X))
    ]).

select_([], X) -> {ok, X};
select_([{element, N} | Tree], X) when is_tuple(X), N =< size(X) ->
    select_(Tree, element(N, X));
select_([{find, P} | Tree], X) when is_list(X) ->
    do([option_m ||
        Y <- xl_lists:find(predicate(P), X),
        select_(Tree, Y)
    ]);
select_([{select, P} | Tree], X) when is_list(X) ->
    monad:flatten(option_m, [select_(Tree, Y) || Y <- lists:filter(predicate(P), X)]);
select_(_Tree, _X) -> undefined.

predicate({Op, A1, A2}) ->
    fun(X) ->
        erlang:Op(argument(A1, X), argument(A2, X))
    end.

argument({element, N}, X) when is_tuple(X), N =< size(X) -> element(N, X);
argument({element, _}, _) -> undefined;
argument({_, X}, _) -> X.

-spec update/4 :: (string(), list(), any(), tuple() | list()) -> error_m:monad(term()).
update(EPath, Params, R, X) -> update(xl_string:format(EPath, Params), R, X).

-spec update/3 :: (string(), any(), tuple() | list()) -> error_m:monad(term()).
update(EPath, R, X) ->
    do([error_m ||
        Tree <- parse(EPath),
        return(update_(Tree, R, X, fun(Value, _) -> Value end))
    ]).

update_([], R, X, Mutate) -> Mutate(R, X);
update_([{element, N} | Tree], R, X, Mutate) when is_tuple(X), N =< size(X) ->
    setelement(N, X, update_(Tree, R, element(N, X), Mutate));
update_([{find, P} | Tree], R, X, Mutate) when is_list(X) ->
    Predicate = predicate(P),
    case lists:splitwith(fun(E) -> not(Predicate(E)) end, X) of
        {F, []} -> F;
        {F, [H|T]} -> lists:concat([F, [update_(Tree, R, H, Mutate)], T])
    end;
update_([{select, P} | Tree], R, X, Mutate) when is_list(X) ->
    Predicate = predicate(P),
    lists:map(fun(E) ->
        case Predicate(E) of
            true -> update_(Tree, R, E, Mutate);
            false -> E
        end
    end, X);
update_(_Tree, _R, X, _Mutate) -> X.

-spec concat/4 :: (string(), list(), any(), tuple() | list()) -> error_m:monad(term()).
concat(EPath, Params, R, X) -> concat(xl_string:format(EPath, Params), R, X).

-spec concat/3 :: (string(), any(), tuple() | list()) -> error_m:monad(term()).
concat(EPath, R, X) ->
    do([error_m ||
        Tree <- parse(EPath),
        return(update_(Tree, R, X, fun(L1, L2) -> L1 ++ L2 end))
    ]).

parse(EPath) ->
    do([error_m ||
        Tokens <- epath_lexer:parse(EPath),
        epath_parser:parse(Tokens)
    ]).