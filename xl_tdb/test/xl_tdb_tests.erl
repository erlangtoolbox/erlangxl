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
    xl_file:delete("/tmp/test/tdbp"),
    xl_application:start(xl_stdlib),
    ?assertOk(xl_tdb:start_link(testtdbp, "/tmp/test/tdbp", xl_tdb:by_index(#testobj.id), [])),
    T1 = #testobj{id = "1", name = "n1"},
    T2 = #testobj{id = "2", name = "n2"},

    ?assertOk(xl_tdb:store(testtdbp, [T1, T2])),
    ?assertEqual({ok, T1}, xl_tdb:get(testtdbp, "1")),
    ?assertEqual({ok, T2}, xl_tdb:get(testtdbp, "2")),
    ?assertEqual(undefined, xl_tdb:get(testtdbp, "3")),

    ?assertOk(xl_tdb:delete(testtdbp, "1")),
    ?assertEqual(undefined, xl_tdb:get(testtdbp, "1")),
    ?assertEqual({ok, T2}, xl_tdb:get(testtdbp, "2")),

    ?assertOk(xl_tdb:delete_all(testtdbp)),
    ?assertEqual([], xl_tdb:select(testtdbp)),
    ?assertEquals(0, xl_tdb:count(testtdbp)),


    ?assertOk(xl_tdb:close(testtdbp)).

update_test() ->
    xl_file:delete("/tmp/test/tdbp"),
    xl_application:start(xl_stdlib),
    ?assertOk(xl_tdb:start_link(testtdbpup, "/tmp/test/tdbp", xl_tdb:by_index(#testobj.id), [])),
    T1 = #testobj{id = "1", name = "n1"},

    ?assertOk(xl_tdb:store(testtdbpup, [T1])),
    ?assertEqual({ok, T1}, xl_tdb:get(testtdbpup, "1")),

    ?assertEqual({error, problem}, xl_tdb:update(testtdbpup, "1", fun(_X) ->
        error(problem)
    end)),

    ?assertEqual({ok, #testobj{id = "1", name = "updated"}}, xl_tdb:update(testtdbpup, "1", fun(X) ->
        {ok, X#testobj{name = "updated"}}
    end)),

    ?assertEqual({ok, undefined}, xl_tdb:update(testtdbpup, "x", fun(undefined) ->
        undefined
    end)),

    ?assertOk(xl_tdb:close(testtdbpup)).

disk_storage_test_() ->
    {timeout, 2000, fun() ->
        xl_file:delete("/tmp/test/tdbp"),
        xl_application:start(xl_stdlib),
        ?assertOk(xl_tdb:start_link(testtdbpds, "/tmp/test/tdbp", xl_tdb:by_index(#testobj.id), [])),
        T1 = #testobj{id = "1", name = "n1"},
        T2 = #testobj{id = "2", name = "n2"},
        xl_eunit:performance(tdb_store, fun() ->
            xl_tdb:store(testtdbpds, [T1, T2])
        end, 100000),
        timer:sleep(500),
        ?assertEqual(ok, xl_tdb:close(testtdbpds)),
        ?assertOk(xl_tdb:start_link(testtdbpds, "/tmp/test/tdbp", xl_tdb:by_index(#testobj.id), [])),
        ?assertEqual([T1, T2], xl_tdb:select(testtdbpds)),
        ?assertEquals(2, xl_tdb:count(testtdbpds)),
        ?assertEqual([T1, T2], xl_stream:to_list(xl_tdb:cursor(testtdbpds))),
        ?assertEqual(ok, xl_tdb:close(testtdbpds))
    end}.

mapfilter_test() ->
    xl_file:delete("/tmp/test/tdbp"),
    xl_application:start(xl_stdlib),
    ?assertOk(xl_tdb:start_link(testtdbpmf, "/tmp/test/tdbp", xl_tdb:by_index(#testobj.id), [
        {index_object, fun index_object/1},
        {index_query, fun index_query/1}
    ])),
    T1 = #testobj{id = "1", name = <<"n1">>},
    T2 = #testobj{id = "2", name = <<"n2">>},
    T3 = #testobj{id = "3", name = <<"n1">>},
    T4 = #testobj{id = "4", name = <<"n3">>},
    ?assertOk(xl_tdb:store(testtdbpmf, [T1, T2, T3, T4])),
    ?assertEquals([T1, T3], xl_tdb:nmapfilter(testtdbpmf, 2, [{name, <<"n1">>}], fun(O) -> {ok, O} end)),
    ?assertEquals([T3], xl_tdb:nmapfilter(testtdbpmf, 1, [{name, <<"n1">>}], fun(O) -> {ok, O} end)),
    ?assertOk(xl_tdb:close(testtdbpmf)).

mapfind_test() ->
    xl_file:delete("/tmp/test/tdbp"),
    xl_application:start(xl_stdlib),
    ?assertOk(xl_tdb:start_link(testtdbpmf, "/tmp/test/tdbp", xl_tdb:by_index(#testobj.id), [
        {index_object, fun index_object/1},
        {index_query, fun index_query/1},
        {index_local_execution, true}
    ])),
    T1 = #testobj{id = "1", name = <<"n1">>},
    T2 = #testobj{id = "2", name = <<"n2">>},
    T3 = #testobj{id = "3", name = <<"n1">>},
    T4 = #testobj{id = "4", name = <<"n3">>},
    ?assertOk(xl_tdb:store(testtdbpmf, [T1, T2, T3, T4])),
    ?assertEquals({ok, T3}, xl_tdb:mapfindc(testtdbpmf, [{name, <<"n1">>}], fun(O, _) -> {ok, O} end, undefined)),
    ?assertOk(xl_tdb:close(testtdbpmf)).

rsync_test() ->
    xl_file:delete("/tmp/test/tdbp"),
    xl_application:start(xl_stdlib),
    ?assertOk(xl_tdb:start_link(testrsync_master, "/tmp/test/tdbp/master", xl_tdb:by_index(#testobj.id), [])),
    T1 = #testobj{id = "1", name = <<"n1">>},
    T2 = #testobj{id = "2", name = <<"n2">>},
    T3 = #testobj{id = "3", name = <<"n1">>},
    T4 = #testobj{id = "4", name = <<"n3">>},
    ?assertOk(xl_tdb:store(testrsync_master, [T1, T2, T3, T4])),
    ?assertOk(xl_tdb:start_link(testrsync_slave, "/tmp/test/tdbp/slave", xl_tdb:by_index(#testobj.id), [
        {rsync_master_node, node()},
        {rsync_master_db, testrsync_master},
        {rsync_treshold, 2},
        {sync, 100}
    ])),
    timer:sleep(500),
    ?assertEquals([T1, T2, T3, T4], xl_tdb:select(testrsync_slave)),
    ?assertEquals(4, xl_tdb:count(testrsync_slave)),

    ?assertEqual({ok, TU = #testobj{id = "1", name = "updated"}},
        xl_tdb:update(testrsync_master, "1", fun(X) -> {ok, X#testobj{name = "updated"}} end)),
    timer:sleep(500),
    ?assertEquals([TU, T2, T3, T4], xl_tdb:select(testrsync_slave)),
    ?assertEquals(4, xl_tdb:count(testrsync_slave)),
    ?assertOk(xl_tdb:close(testrsync_slave)),
    ?assertOk(xl_tdb:close(testrsync_master)).


index_object(O = #testobj{name = Name}) -> [{Name, O}].
index_query([{name, Name}]) -> {Name}.

migration_test() ->
    lists:foreach(fun(Dir) ->
        xl_file:delete("/tmp/test"),
        xl_eunit:explode(?MODULE, Dir, "/tmp/test"),
        xl_application:start(xl_stdlib),
        ?assertOk(xl_tdb:start_link(tdb_migration, "/tmp/test/" ++ Dir, xl_tdb:by_index(1), [
            {version, 3},
            {migrations, [
                {2, fun migrate2/1},
                {3, fun migrate3/1}
            ]}
        ])),
        ?assertEquals([{"1", "Comment"}, {"2", "Comment"}], xl_tdb:select(tdb_migration)),
        ?assertEquals(2, xl_tdb:count(tdb_migration)),
        ?assertEquals({ok, false}, xl_file:exists("/tmp/test/" ++ Dir ++ "/.version")),
        ?assertOk(xl_tdb:close(tdb_migration))
    end, ["tdb", "tdb.new"]).

migrate2({Id, Name}) -> {ok, {Id, Name, "Comment"}}.
migrate3({Id, _Name, Comment}) -> {ok, {Id, Comment}}.
