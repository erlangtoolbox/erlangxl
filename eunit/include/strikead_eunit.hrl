-define(assertFilesEqual(File1, File2),
    begin
        R1 = {S1, _} = file:read_file(File1),
        R2 = {S2, _} = file:read_file(File2),
        ?assertEqual(ok, S1),
        ?assertEqual(ok, S2),
        ?assertEqual(R1, R2)
    end).
