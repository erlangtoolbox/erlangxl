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
-module(xl_tdbp_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_stdlib/include/xl_eunit.hrl").

-record(testobj, {
    id,
    name
}).

memory_options_test() ->
    xl_application:start(xl_stdlib),
    xl_tdbp:open(testtdbp, "/tmp/test/tdbp", xl_tdbp:by_index(#testobj.id), []),
    T1 = #testobj{id = "1", name = "n1"},
    T2 = #testobj{id = "2", name = "n2"},

    ?assertOk(xl_tdbp:store(testtdbp, [T1, T2])),
    ?assertEqual({ok, T1}, xl_tdbp:get(testtdbp, "1")),
    ?assertEqual({ok, T2}, xl_tdbp:get(testtdbp, "2")),
    ?assertEqual(undefined, xl_tdbp:get(testtdbp, "3")),

    ?assertOk(xl_tdbp:delete(testtdbp, "1")),
    ?assertEqual(undefined, xl_tdbp:get(testtdbp, "1")),
    ?assertEqual({ok, T2}, xl_tdbp:get(testtdbp, "2")),

    ?assertOk(xl_tdbp:close(testtdbp)).

disk_storage_test() ->
    xl_application:start(xl_stdlib),
    xl_tdbp:open(testtdbpds, "/tmp/test/tdbp", xl_tdbp:by_index(#testobj.id), []),
    T1 = #testobj{id = "1", name = "n1"},
    T2 = #testobj{id = "2", name = "n2"},
    xl_tdbp:store(testtdbpds, [T1, T2]),
    timer:sleep(500),
    ?assertEqual(ok, xl_tdbp:close(testtdbpds)),
    xl_tdbp:open(testtdbpds, "/tmp/test/tdbp", xl_tdbp:by_index(#testobj.id), []),
    ?assertEqual([T1, T2], xl_tdbp:select(testtdbpds)),
    ?assertEqual(ok, xl_tdbp:close(testtdbpds)).

mapfilter_test() ->
    xl_application:start(xl_stdlib),
    xl_tdbp:open(testtdbpmf, "/tmp/test/tdbp", xl_tdbp:by_index(#testobj.id), [
        {index_object, fun index_object/1},
        {index_query, fun index_query/1}
    ]),
    T1 = #testobj{id = "1", name = <<"n1">>},
    T2 = #testobj{id = "2", name = <<"n2">>},
    T3 = #testobj{id = "3", name = <<"n1">>},
    T4 = #testobj{id = "4", name = <<"n3">>},
    ?assertOk(xl_tdbp:store(testtdbpmf, [T1, T2, T3, T4])),
    ?assertEquals([T3, T1], xl_tdbp:nmapfilter(testtdbpmf, 2, [{name, <<"n1">>}], fun(O) -> {ok, O} end)),
    ?assertOk(xl_tdbp:close(testtdbpmf)),
    ?assertEquals([T1], xl_tdbp:nmapfilter(testtdbpmf, 1, [{name, <<"n1">>}], fun(O) -> {ok, O} end)),
    ?assertOk(xl_tdbp:close(testtdbpmf)).


index_object(O = #testobj{name = Name}) -> [{Name, O}].
index_query([{name, Name}]) -> {Name}.
