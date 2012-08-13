-module(strikead_file_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("strikead_eunit/include/strikead_eunit.hrl").

copy_file_test() ->
    ok = strikead_file:delete("/tmp/test"),
    ok = strikead_file:write_file("/tmp/test/x", "data"),
    ?assertEqual(ok, strikead_file:copy("/tmp/test/x", "/tmp/test/y")),
    ?assertFilesEqual("/tmp/test/x", "/tmp/test/y/x"),
    ok = strikead_file:delete("/tmp/test").

copy_recursive_test() ->
    ok = strikead_file:delete("/tmp/test"),
    ok = strikead_file:write_file("/tmp/test/a/a", "data"),
    ok = strikead_file:write_file("/tmp/test/a/b/c", "data"),
    ok = strikead_file:write_file("/tmp/test/a/b/d", "data"),
    ok = strikead_file:mkdirs("/tmp/test/a/b/e"),

    ?assertEqual(ok, strikead_file:copy("/tmp/test/a", "/tmp/test/y")),
    ?assertFilesEqual("/tmp/test/a/a", "/tmp/test/y/a/a"),
    ?assertFilesEqual("/tmp/test/a/b/c", "/tmp/test/y/a/b/c"),
    ?assertFilesEqual("/tmp/test/a/b/c", "/tmp/test/y/a/b/d"),
    ?assert(filelib:is_dir("/tmp/test/y/a/b/e")),
    ok = strikead_file:delete("/tmp/test").

write_terms_test() ->
    ok = strikead_file:delete("/tmp/test"),
    ?assertEqual(ok, strikead_file:write_terms("/tmp/test/x", a)),
    ?assertEqual({ok, [a]}, strikead_file:read_terms("/tmp/test/x")),
    ok = strikead_file:delete("/tmp/test").

read_files_test() ->
	ok = strikead_file:write_file("/tmp/test/1/a", "dataa"),
	ok = strikead_file:write_file("/tmp/test/2/b", "datab"),
	ok = strikead_file:write_file("/tmp/test/2/c", "datac"),
	?assertEqual({ok, [{"a", <<"dataa">>}, {"b", <<"datab">>}, {"c", <<"datac">>}]},
		strikead_file:read_files(["/tmp/test/1/*","/tmp/test/2/*"])),
    ok = strikead_file:delete("/tmp/test").

read_files_base_test() ->
	ok = strikead_file:write_file("/tmp/test/2/b", "datab"),
	ok = strikead_file:write_file("/tmp/test/2/c", "datac"),
	?assertEqual({ok, [{"2/b", <<"datab">>}, {"2/c", <<"datac">>}]},
		strikead_file:read_files(["/tmp/test/2/*"], {base, "/tmp/test"})),
	?assertEqual({ok, [{"2/b", <<"datab">>}, {"2/c", <<"datac">>}]},
		strikead_file:read_files(["/tmp/test/2/*"], {base, "/tmp/test/"})),
    ok = strikead_file:delete("/tmp/test").

