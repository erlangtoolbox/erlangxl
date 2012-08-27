-define(assertFilesEqual(File1, File2),
begin
    ?assertEqual({ok, true}, xl_file:exists(File1)),
    ?assertEqual({ok, true}, xl_file:exists(File2)),
    ?assertEqual(xl_file:read_file(File1), xl_file:read_file(File2))
end).

-define(assertOk(X), ?assertEqual(ok, element(1, X))).

