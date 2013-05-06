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
-module(xl_tdb_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_stdlib/include/xl_eunit.hrl").

-record(testobj, {
    id,
    name
}).

memory_options_test() ->
    xl_application:start(xl_stdlib),
    {ok, Pid} = xl_tdb:open("/tmp/test/tdb", xl_tdb:by_index(#testobj.id), [{fsync_interval, 100}]),
    T1 = #testobj{id = "1", name = "n1"},
    T2 = #testobj{id = "2", name = "n2"},

    ?assertOk(xl_tdb:store(Pid, [T1, T2])),
    ?assertEqual({ok, T1}, xl_tdb:get(Pid, "1")),
    ?assertEqual({ok, T2}, xl_tdb:get(Pid, "2")),
    ?assertEqual(undefined, xl_tdb:get(Pid, "3")),

    ?assertOk(xl_tdb:delete(Pid, "1")),
    ?assertEqual(undefined, xl_tdb:get(Pid, "1")),
    ?assertEqual({ok, T2}, xl_tdb:get(Pid, "2")),

    ?assertEqual(ok, xl_tdb:close(Pid)).

dis_storage_test() ->
    {ok, Pid} = xl_tdb:open("/tmp/test/tdb", xl_tdb:by_index(#testobj.id), [{fsync_interval, 100}]),
    T1 = #testobj{id = "1", name = "n1"},
    T2 = #testobj{id = "2", name = "n2"},
    xl_tdb:store(Pid, [T1, T2]),
    timer:sleep(500),
    ?assertEqual(ok, xl_tdb:close(Pid)),
    {ok, Pid2} = xl_tdb:open("/tmp/test/tdb", xl_tdb:by_index(#testobj.id), [{fsync_interval, 100}]),
    ?assertEqual([T1, T2], xl_tdb:select(Pid2)),
    ?assertEqual(ok, xl_tdb:close(Pid2)).