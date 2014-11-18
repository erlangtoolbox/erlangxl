%% =============================================================================
%%  The MIT License (MIT)
%%
%%  Copyright (c) 2014 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy of
%%  this software and associated documentation files (the "Software"), to deal in
%%  the Software without restriction, including without limitation the rights to
%%  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
%%  the Software, and to permit persons to whom the Software is furnished to do so,
%%  subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in all
%%  copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
%%  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
%%  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
%%  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%%  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%% =============================================================================

-module(xl_code_escript_tests).

-include_lib("eunit/include/eunit.hrl").

read_file_test() ->
    ok = xl_file:delete("/tmp/test"),
    {ok, {_, Zip}} = zip:create("mem", [{"x.txt", <<"xxx">>}, {"a/b/c.txt", <<"cccc">>}], [memory]),
    {ok, EScript} = escript:create(binary, [
        {shebang, default},
        {comment, default},
        {emu_args, undefined},
        {archive, Zip}
    ]),
    File = "/tmp/test/testescript",
    ok = xl_file:write(File, EScript),
    ?assertEqual({ok, <<"xxx">>}, xl_code_escript:read_file(File, "x.txt")).

unpack_priv_test() ->
    ok = xl_file:delete("/tmp/test"),
    {ok, {_, Zip}} = zip:create("mem", [{"priv/x.txt", <<"xxx">>}, {"a/b/c.txt", <<"cccc">>}], [memory]),
    {ok, EScript} = escript:create(binary, [
        {shebang, default},
        {comment, default},
        {emu_args, undefined},
        {archive, Zip}
    ]),
    File = "/tmp/test/testescript",
    ok = xl_file:write(File, EScript),
    ?assertEqual(ok, xl_code_escript:unpack_priv(File, "/tmp/test/xx/testescript")),
    ?assertEqual({ok, true}, xl_file:exists("/tmp/test/xx/testescript/priv/x.txt")).
