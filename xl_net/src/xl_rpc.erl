%% Copyright
-module(xl_rpc).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([call/4, call/5]).

-spec call/4 :: (node(), atom(), atom(), [term()]) -> error_m:monad(term()).
call(Node, Module, Function, Args) ->
    call(Node, Module, Function, Args, infinity).

-spec call/5 :: (node(), atom(), atom(), [term()], timeout()) -> error_m:monad(term()).
call(Node, Module, Function, Args, Timeout) ->
    case rpc:call(Node, Module, Function, Args, Timeout) of
        {ok, R} -> {ok, R};
        {badrpc, R} -> {error, R};
        R -> {ok, R}
    end.