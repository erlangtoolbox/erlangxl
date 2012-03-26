-define(assertFilesEqual(File1, File2),
	begin
		?assert(strikead_file:exists(File1)),
		?assert(strikead_file:exists(File2)),
		?assertEqual(strikead_file:read_file(File1), strikead_file:read_file(File2))
	end).

-define(assertOk(X), ?assertEqual(ok, element(1, X))).
