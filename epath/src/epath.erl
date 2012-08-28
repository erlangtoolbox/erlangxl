%% Copyright
-module(epath).
-author("Volodymyr Kyrychenko <volodymyr.kyrychenko@strikead.com>").

-compile({parse_transform, do}).

%% API
-export([select/2]).

-spec select/2 :: (string(), tuple() | list()) -> error_m:monad(option_m:monad(term())).
select(EPath, X) ->
    do([error_m ||
        Tokens <- epath_lexer:parse(EPath),
        Tree <- epath_parser:parse(Tokens),
        return(select_(Tree, X))
    ]).

select_([], X) -> {ok, X};
select_([{element, N} | Tree], X) when is_tuple(X), size(X) =< N ->
    select_(element(N, X), Tree);
select_([{find, P} | Tree], X) when is_list(X) ->
    do([option_m ||
        Y <- xl_lists:find(predicate(P), X),
        select_(Tree, Y)
    ]);
select_(_, _) -> undefined.

predicate({Op, A1, A2}) ->
    fun(X) ->
        erlang:display(X),
        erlang:Op(argument(A1, X), argument(A2, X))
    end.

argument({element, N}, X) when is_tuple(X), size(X) =< N -> element(N, X);
argument({element, _}, _) -> undefined;
argument({_, X}, _) -> X.

