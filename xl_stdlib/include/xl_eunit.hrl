%% Copyright

-include_lib("eunit/include/eunit.hrl").

-define(assertFilesEqual(File1, File2), begin
    ?assertEquals({ok, true}, xl_file:exists(File1)),
    ?assertEquals({ok, true}, xl_file:exists(File2)),
    ?assertEquals(xl_file:read_file(File1), xl_file:read_file(File2))
end).

-define(assertOk(Expr),
    ((fun(X) ->
        case X of
            ok -> ok;
            _ when element(1, X) == ok -> ok;
            _ -> erlang:error({assertOk_failed, X})
        end
    end)(Expr))
).

-define(assertEquals(ExpectExpr, Expr),
    ((fun(X) ->
        Expect = ExpectExpr,
        case X of
            Expect -> ok;
            Value ->
                xl_eunit:format("\033[31m\033[1mFAILED\033[0m ~s (~p)~nexpression: ~s~nexpected: ~9000p~n but was:\033[31m ~9000p~n\033[0m", [?FILE, ?LINE, ??Expr, Expect, Value]),
                erlang:error(assertEquals_failed)
        end
    end)(Expr))
).

-define(assertMatches(Guard, Expr),
    ((fun () ->
        case (Expr) of
            Guard -> ok;
            Value ->
                xl_eunit:format("\033[31m\033[1mFAILED\033[0m ~s (~p)~nexpression: ~s~nexpected: ~s~n but was:\033[31m ~9000p~n\033[0m", [?FILE, ?LINE, ??Expr, ??Guard, Value]),
                erlang:error(assertMatch_failed)
        end
    end)())).