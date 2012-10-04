-define(assertFilesEqual(File1, File2), begin
    ?assertEqual({ok, true}, xl_file:exists(File1)),
    ?assertEqual({ok, true}, xl_file:exists(File2)),
    ?assertEqual(xl_file:read_file(File1), xl_file:read_file(File2))
end).

-define(assertOk(Expr),
    ((fun(X) ->
        case X of
            ok -> ok;
            _ ->
                case element(1, X) of
                    ok -> ok;
                    _ -> erlang:error({assertOk_failed, X})
                end
        end
    end)(Expr))
).

-define(assertEquals(Expect, Expr),
    ((fun(X) ->
        case X of
            Expect -> ok;
            Value ->
                erlang:display({module, ?MODULE}),
                erlang:display({line, ?LINE}),
                erlang:display({expression, ??Expr}),
                erlang:display('expected:'),
                erlang:display(xl_string:unquote(xl_string:format("~9000p", [Expect]))),
                erlang:display('value:'),
                erlang:display(xl_string:unquote(xl_string:format("~9000p", [Value]))),
                erlang:error(assertEqual_failed)
        end
    end)(Expr))
).
