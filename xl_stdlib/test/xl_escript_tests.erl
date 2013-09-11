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
-module(xl_escript_tests).

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
    ok = xl_file:write_file(File, EScript),
    ?assertEqual({ok, <<"xxx">>}, xl_escript:read_file(File, "x.txt")).

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
    ok = xl_file:write_file(File, EScript),
    ?assertEqual(ok, xl_escript:unpack_priv(File, "/tmp/test/xx/testescript")),
    ?assertEqual({ok, true}, xl_file:exists("/tmp/test/xx/testescript/priv/x.txt")).
