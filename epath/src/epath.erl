%% Copyright
-module(epath).
-author("Volodymyr Kyrychenko <volodymyr.kyrychenko@strikead.com>").

-compile({parse_transform, do}).

%% API
-export([select/2, select/3, update/3, update/4]).

-spec select/3 :: (string(), list(), tuple() | list()) -> error_m:monad(option_m:monad(term())).
select(EPath, Params, X) -> select(xl_string:format(EPath, Params), X).

-spec select/2 :: (string(), tuple() | list()) -> error_m:monad(option_m:monad(term())).
select(EPath, X) ->
    do([error_m ||
        Tokens <- epath_lexer:parse(EPath),
        Tree <- epath_parser:parse(Tokens),
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
select_(_Tree, _X) -> {ok, undefined}.

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
        Tokens <- epath_lexer:parse(EPath),
        Tree <- epath_parser:parse(Tokens),
        return(update_(Tree, R, X))
    ]).

update_([], R, _X) -> R;
update_([{element, N} | Tree], R, X) when is_tuple(X), N =< size(X) ->
    setelement(N, X, update_(Tree, R, element(N, X)));
update_([{find, P} | Tree], R, X) when is_list(X) ->
    Predicate = predicate(P),
    lists:map(fun(E) ->
        case Predicate(E) of
            true -> update_(Tree, R, E);
            false -> E
        end
    end, X);
update_(_Tree, _R, X) -> X.
