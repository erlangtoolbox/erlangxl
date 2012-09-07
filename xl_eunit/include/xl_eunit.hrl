-define(assertFilesEqual(File1, File2), begin
    ?assertEqual({ok, true}, xl_file:exists(File1)),
    ?assertEqual({ok, true}, xl_file:exists(File2)),
    ?assertEqual(xl_file:read_file(File1), xl_file:read_file(File2))
end).

-define(assertOk(X), ?assertEqual(ok, element(1, X))).

-define(assertEquals(Expect, Expr),
    ((fun() ->
        case (Expr) of
            Expect -> ok;
            Value ->
                erlang:display({module, ?MODULE}),
                erlang:display({line, ?LINE}),
                erlang:display({expression, ??Expr}),
                erlang:display('expected:'),
                erlang:display(Expect),
                erlang:display('value:'),
                erlang:display(Value),
                erlang:error(assertEqual_failed)
        end
    end)())
).
