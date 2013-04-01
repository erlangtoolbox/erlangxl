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
-module(xl_flog_tests).

-include_lib("eunit/include/eunit.hrl").

tsv_format_test() ->
    os:cmd("rm -r /tmp/test"),
    xl_flog:start_link(tsv, "/tmp/test", fun xl_flog:format_tsv/2),
    xl_flog:log(tsv, [a, "b", 1]),
    xl_flog:log(tsv, [a, b, 1]),
    xl_flog:log(tsv, [a, b, c]),
    xl_flog:flush(tsv),
    xl_flog:stop(tsv),
    ?assertEqual({ok, <<"a\tb\t1\na\tb\t1\na\tb\tc\n">>}, xl_file:read_file(find_file())).

tsv_format_empty_string_test() ->
    os:cmd("rm -r /tmp/test"),
    xl_flog:start_link(tsv_empty, "/tmp/test", fun xl_flog:format_tsv/2),
    xl_flog:log(tsv_empty, [a, "", 1]),
    xl_flog:flush(tsv_empty),
    xl_flog:stop(tsv_empty),
    ?assertEqual({ok, <<"a\t\t1\n">>}, xl_file:read_file(find_file())).

tsv_terms_format_test() ->
    os:cmd("rm -r /tmp/test"),
    xl_flog:start_link(tsv_terms, "/tmp/test", fun xl_flog:format_tsv/2),
    xl_flog:log(tsv_terms, [a, [{1, "1"}], 1]),
    xl_flog:flush(tsv_terms),
    xl_flog:stop(tsv_terms),
    ?assertEqual({ok, <<"a\t[{1,\"1\"}]\t1\n">>}, xl_file:read_file(find_file())).

terms_long_no_linebreak_test() ->
    os:cmd("rm -r /tmp/test"),
    xl_flog:start_link(terms, "/tmp/test", fun xl_flog:format_terms/2),
    xl_flog:log(terms, [aaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbb, ccccccccccccccccccccc, dddddddddddddddddddddddddd]),
    xl_flog:log(terms, [a, b, c]),
    xl_flog:log(terms, [a, b, c]),
    xl_flog:flush(terms),
    xl_flog:stop(terms),
    {ok, R} = xl_file:read_file(find_file()),
    ?assertEqual(<<"[aaaaaaaaaaaaaaaaaaaaaaaaaaa,bbbbbbbbbbbbbbbbbbbbbbbb,ccccccccccccccccccccc,dddddddddddddddddddddddddd]\n[a,b,c]\n[a,b,c]\n">>, R).

find_file() ->
    {ok, [D]} = xl_file:list_dir("/tmp/test/"),
    {ok, [F]} = xl_file:list_dir("/tmp/test/" ++ D),
    "/tmp/test/" ++ D ++ "/" ++ F.

