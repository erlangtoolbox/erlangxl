%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @copyright (C) 2013, strikead.com
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xl_memdb_tests).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-include_lib("xl_stdlib/include/xl_eunit.hrl").

memdb_test() ->
    ?assertOk(xl_application:start(xl_stdlib)),
    xl_memdb:start(testmemdb),
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

dump_load_test() ->
    ?assertOk(xl_application:start(xl_stdlib)),
    xl_memdb:start(testmemdb),
    xl_memdb:store(testmemdb, {a, 1}, a1),
    xl_memdb:store(testmemdb, {a, 2}, b1),
    xl_memdb:store(testmemdb, {b, 1}, a2),
    xl_memdb:store(testmemdb, {b, 2}, b2),
    xl_memdb:dump(testmemdb, "/tmp/test/dump.memdb"),
    xl_memdb:stop(testmemdb),
    xl_memdb:start(testmemdb2),
    xl_memdb:load(testmemdb2, "/tmp/test/dump.memdb"),
    ?assertEquals({ok, a1}, xl_memdb:get(testmemdb2, {a, 1})),
    ?assertEquals({ok, a2}, xl_memdb:get(testmemdb2, {b, 1})),
    xl_memdb:stop(testmemdb2).

replication_test() ->
    ?assertOk(xl_application:start(xl_stdlib)),
    ?assertOk(xl_memdb:start(testrsync_master)),
    xl_memdb:store(testrsync_master, 1, a),
    xl_memdb:store(testrsync_master, 2, b),
    xl_memdb:store(testrsync_master, 3, c),
    xl_memdb:store(testrsync_master, 4, d),
    ?assertOk(xl_memdb:start(testrsync_slave, [
        {replication_safe_interval, 100},
        {replication_master_node, node()},
        {replication_master_db, testrsync_master},
        {replication_bucket, 2},
        {replication_interval, 100}
    ])),
    timer:sleep(300),
    ?assertEquals([{1, a}, {2, b}, {3, c}, {4, d}], lists:usort(xl_memdb:items(testrsync_slave))),
    xl_memdb:store(testrsync_master, 3, ccc),
    timer:sleep(300),
    ?assertEquals([{1, a}, {2, b}, {3, ccc}, {4, d}], lists:usort(xl_memdb:items(testrsync_slave))),
    ?assertOk(xl_memdb:stop(testrsync_slave)),
    ?assertOk(xl_memdb:stop(testrsync_master)).
