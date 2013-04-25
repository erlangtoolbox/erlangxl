%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

write_term_test() ->
    ok = xl_file:delete("/tmp/test"),
    ?assertEqual(ok, xl_file:write_term("/tmp/test/x", a)),
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
