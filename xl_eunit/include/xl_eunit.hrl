-define(assertFilesEqual(File1, File2), begin
    ?assertEqual({ok, true}, xl_file:exists(File1)),
    ?assertEqual({ok, true}, xl_file:exists(File2)),
    ?assertEqual(xl_file:read_file(File1), xl_file:read_file(File2))
end).

-define(assertOk(X), ?assertEqual(ok, element(1, X))).

-define(assertEquals(X, Y), begin
    try
        ?assertEqual(X, Y)
    catch
        error:{assertEqual_failed, [M, L, Ex, {expected, E}, {value, V}]} ->
            erlang:display(M),
            erlang:display(L),
            erlang:display(Ex),
            erlang:display('expected:'),
            erlang:display(E),
            erlang:display('value:'),
            erlang:display(V),
            error(assertEqual_failed)
    end
end).