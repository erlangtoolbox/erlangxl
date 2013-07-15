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
-module(xl_lang_tests).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-include_lib("eunit/include/eunit.hrl").
-include("xl_lang.hrl").

-record(r, {a, b, c}).

record_to_proplist_test() ->
    R = #r{a = 1, b = 2, c = c},
    ?assertEqual([{a, 1}, {b, 2}, {c, c}], ?RECORD_TO_PROPLIST(R, r)).

safe_call_fail_test() ->
    ?assertEqual(result, xl_lang:safe_call(fun() -> error(fail) end, result)).

safe_call_fail_with_fun_test() ->
    ?assertEqual(result, xl_lang:safe_call(fun() -> error(fail) end, fun() -> result end)).

safe_call_success_test() ->
    ?assertEqual(ok, xl_lang:safe_call(fun() -> ok end, result)).

register_test() ->
    ?assertEqual({error, {cannot_register, xxx, yyy}}, xl_lang:register(xxx, yyy)),
    Pid = spawn_link(fun safe_call_fail_test/0),
    ?assertEqual({ok, Pid}, xl_lang:register(xxx, Pid)).

delete_element_test() ->
    ?assertEqual({1, 3}, xl_lang:delete_element(2, {1, 2, 3})).

insert_element_test() ->
    ?assertEqual({1, x, 2, 3}, xl_lang:insert_element(2, x, {1, 2, 3})).
