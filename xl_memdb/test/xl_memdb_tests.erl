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
-module(xl_memdb_tests).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-include_lib("xl_stdlib/include/xl_eunit.hrl").

complex_test() ->
    Configs = [[{memory, async}], [{memory, sync}]],
    lists:foreach(fun(Options) ->
        error_logger:info_msg("Config ~p~n", [Options]),
        basic_test(Options),
        dump_load_test(Options),
        update_test(Options),
        replication_test(Options),
        migration_test(Options)
    end, Configs).

basic_test(Options) ->
    ?assertOk(xl_application:start(xl_stdlib)),
    xl_memdb:start(testmemdb, Options),
    xl_memdb:store(testmemdb, {a, 1}, a1),
    xl_memdb:store(testmemdb, {a, 2}, b1),
    Since = xl_calendar:now_micros(),
    xl_memdb:store(testmemdb, {b, 1}, a2),
    xl_memdb:store(testmemdb, {b, 2}, b2),
    ?assertEquals({ok, a1}, xl_memdb:get(testmemdb, {a, 1})),
    ?assertEquals({ok, a2}, xl_memdb:get(testmemdb, {b, 1})),
    ?assertEquals([{{a, 1}, a1}, {{a, 2}, b1}, {{b, 1}, a2}, {{b, 2}, b2}], lists:usort(xl_memdb:items(testmemdb))),
    ?assertMatches([{{b, 1}, a2, _}, {{b, 2}, b2, _}], lists:usort(xl_stream:to_list(xl_memdb:updates(testmemdb, Since)))),
    xl_memdb:stop(testmemdb).

dump_load_test(Options) ->
    ?assertOk(xl_application:start(xl_stdlib)),

    xl_memdb:start(testmemdb, Options),
    xl_memdb:store(testmemdb, {a, 1}, a1),
    xl_memdb:store(testmemdb, {a, 2}, b1),
    xl_memdb:store(testmemdb, {b, 1}, a2),
    xl_memdb:store(testmemdb, {b, 2}, b2),
    xl_memdb:dump(testmemdb, "/tmp/test/dump.memdb"),
    xl_memdb:stop(testmemdb),

    xl_memdb:start(testmemdb2, Options),
    xl_memdb:load(testmemdb2, "/tmp/test/dump.memdb"),
    ?assertEquals({ok, a1}, xl_memdb:get(testmemdb2, {a, 1})),
    ?assertEquals({ok, a2}, xl_memdb:get(testmemdb2, {b, 1})),
    xl_memdb:stop(testmemdb2).

migration_test(Options) ->
    ?assertOk(xl_application:start(xl_stdlib)),

    ets:new(old_format, [public, named_table, set]),
    true = ets:insert(old_format, [{{a, 1}, a1}, {{b, 1}, a2}]),
    ets:tab2file(old_format, "/tmp/test/dump.memdb"),
    ets:delete(old_format),

    xl_memdb:start(testmemdb2, Options),
    xl_memdb:load(testmemdb2, "/tmp/test/dump.memdb"),
    ?assertEquals({ok, a1}, xl_memdb:get(testmemdb2, {a, 1})),
    ?assertEquals({ok, a2}, xl_memdb:get(testmemdb2, {b, 1})),
    xl_memdb:stop(testmemdb2).

update_test(Options) ->
    ?assertOk(xl_application:start(xl_stdlib)),
    ?assertOk(xl_memdb:start(testmemdb, Options)),

    xl_memdb:store(testmemdb, {a, 1}, a1),
    ?assertEquals({ok, a1}, xl_memdb:get(testmemdb, {a, 1})),

    ?assertEqual({error, problem}, xl_memdb:update(testmemdb, {a, 1}, fun(_X) ->
        error(problem)
    end)),

    ?assertEqual({ok, a2}, xl_memdb:update(testmemdb, {a, 1}, fun(_X) ->
        {ok, a2}
    end)),
    ?assertEquals({ok, a2}, xl_memdb:get(testmemdb, {a, 1})),

    ?assertEqual({ok, undefined}, xl_memdb:update(testmemdb, x, fun(_X) ->
        undefined
    end)),

    ?assertOk(xl_memdb:stop(testmemdb)).


replication_test(Options) ->
    ?assertOk(xl_application:start(xl_stdlib)),
    ?assertOk(xl_memdb:start(memdbrsync_master, Options)),
    xl_memdb:store(memdbrsync_master, 1, a),
    xl_memdb:store(memdbrsync_master, 2, b),
    xl_memdb:store(memdbrsync_master, 3, c),
    xl_memdb:store(memdbrsync_master, 4, d),
    ?assertOk(xl_memdb:start(memdbrsync_slave, [
        {replication_safe_interval, 100},
        {replication_master_node, node()},
        {replication_master_db, memdbrsync_master},
        {replication_bucket, 2},
        {replication_interval, 100} | Options
    ])),
    timer:sleep(300),
    ?assertEquals([{1, a}, {2, b}, {3, c}, {4, d}], lists:usort(xl_memdb:items(memdbrsync_slave))),
    xl_memdb:store(memdbrsync_master, 3, ccc),
    timer:sleep(300),
    ?assertEquals([{1, a}, {2, b}, {3, ccc}, {4, d}], lists:usort(xl_memdb:items(memdbrsync_slave))),
    ?assertOk(xl_memdb:stop(memdbrsync_slave)),
    ?assertOk(xl_memdb:stop(memdbrsync_master)).
