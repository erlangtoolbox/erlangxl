-module(strikead_file_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("strikead_eunit/include/strikead_eunit.hrl").

copy_file_test() ->
    os:cmd("rm -rf /tmp/test"),
    ok = strikead_file:write_file("/tmp/test/x", "data"),
    ?assertEqual(ok, strikead_file:copy("/tmp/test/x", "/tmp/test/y")),
    ?assertFilesEqual("/tmp/test/x", "/tmp/test/y/x"),
    os:cmd("rm -rf /tmp/test").

copy_recursive_test() ->
    os:cmd("rm -rf /tmp/test"),
    ok = strikead_file:write_file("/tmp/test/a/a", "data"),
    ok = strikead_file:write_file("/tmp/test/a/b/c", "data"),
    ok = strikead_file:write_file("/tmp/test/a/b/d", "data"),
    ok = strikead_file:mkdirs("/tmp/test/a/b/e"),

    ?assertEqual(ok, strikead_file:copy("/tmp/test/a", "/tmp/test/y")),
    ?assertFilesEqual("/tmp/test/a/a", "/tmp/test/y/a/a"),
    ?assertFilesEqual("/tmp/test/a/b/c", "/tmp/test/y/a/b/c"),
    ?assertFilesEqual("/tmp/test/a/b/c", "/tmp/test/y/a/b/d"),
    ?assert(filelib:is_dir("/tmp/test/y/a/b/e")),
    os:cmd("rm -rf /tmp/test").

write_terms_test() ->
	os:cmd("rm -rf /tmp/test"),
	?assertEqual(ok, strikead_file:write_terms("/tmp/test/x", a)),
	?assertEqual({ok, [a]}, strikead_file:read_terms("/tmp/test/x")),
	os:cmd("rm -rf /tmp/test").
