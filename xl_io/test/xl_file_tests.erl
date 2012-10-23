-module(xl_file_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_stdlib/include/xl_eunit.hrl").

copy_file_test() ->
    ok = xl_file:delete("/tmp/test"),
    ok = xl_file:write_file("/tmp/test/x", "data"),
    ?assertEqual(ok, xl_file:copy("/tmp/test/x", "/tmp/test/y")),
    ?assertFilesEqual("/tmp/test/x", "/tmp/test/y/x"),
    ok = xl_file:delete("/tmp/test").

copy_recursive_test() ->
    ok = xl_file:delete("/tmp/test"),
    ok = xl_file:write_file("/tmp/test/a/a", "data"),
    ok = xl_file:write_file("/tmp/test/a/b/c", "data"),
    ok = xl_file:write_file("/tmp/test/a/b/d", "data"),
    ok = xl_file:mkdirs("/tmp/test/a/b/e"),

    ?assertEqual(ok, xl_file:copy("/tmp/test/a", "/tmp/test/y")),
    ?assertFilesEqual("/tmp/test/a/a", "/tmp/test/y/a/a"),
    ?assertFilesEqual("/tmp/test/a/b/c", "/tmp/test/y/a/b/c"),
    ?assertFilesEqual("/tmp/test/a/b/c", "/tmp/test/y/a/b/d"),
    ?assert(filelib:is_dir("/tmp/test/y/a/b/e")),
    ok = xl_file:delete("/tmp/test").

write_terms_test() ->
    ok = xl_file:delete("/tmp/test"),
    ?assertEqual(ok, xl_file:write_terms("/tmp/test/x", a)),
    ?assertEqual({ok, [a]}, xl_file:read_terms("/tmp/test/x")),
    ok = xl_file:delete("/tmp/test").

read_files_test() ->
    ok = xl_file:write_file("/tmp/test/1/a", "dataa"),
    ok = xl_file:write_file("/tmp/test/2/b", "datab"),
    ok = xl_file:write_file("/tmp/test/2/c", "datac"),
    ?assertEqual({ok, [{"a", <<"dataa">>}, {"b", <<"datab">>}, {"c", <<"datac">>}]},
        xl_file:read_files(["/tmp/test/1/*", "/tmp/test/2/*"])),
    ok = xl_file:delete("/tmp/test").

read_files_base_test() ->
    ok = xl_file:write_file("/tmp/test/2/b", "datab"),
    ok = xl_file:write_file("/tmp/test/2/c", "datac"),
    ?assertEqual({ok, [{"2/b", <<"datab">>}, {"2/c", <<"datac">>}]},
        xl_file:read_files(["/tmp/test/2/*"], {base, "/tmp/test"})),
    ?assertEqual({ok, [{"2/b", <<"datab">>}, {"2/c", <<"datac">>}]},
        xl_file:read_files(["/tmp/test/2/*"], {base, "/tmp/test/"})),
    ?assertEqual({ok, [{"2/b", <<"datab">>}, {"2/c", <<"datac">>}]},
        xl_file:read_files(["/tmp/test/2"], {base, "/tmp/test/"})),
    ?assertEqual({ok, [{"2/b", <<"datab">>}, {"2/c", <<"datac">>}]},
        xl_file:read_files(["/tmp/test/2"], {base, "/tmp/test/."})),
    ok = xl_file:delete("/tmp/test").

absolute_test() ->
    ?assertEqual("/a/b/c", xl_file:absolute("/a/b/c/d/e/./../../.")),
    ?assertEqual("/", xl_file:absolute("/a/b/../../../../..")).
