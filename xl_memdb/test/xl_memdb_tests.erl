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
    xl_memdb:store(testmemdb, {b, 1}, a2),
    xl_memdb:store(testmemdb, {b, 2}, b2),
    ?assertEquals({ok, {{a, 1}, a1}}, xl_memdb:get(testmemdb, {a, 1})),
    ?assertEquals({ok, {{b, 1}, a2}}, xl_memdb:get(testmemdb, {b, 1})),
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
    ?assertEquals({ok, {{a, 1}, a1}}, xl_memdb:get(testmemdb2, {a, 1})),
    ?assertEquals({ok, {{b, 1}, a2}}, xl_memdb:get(testmemdb2, {b, 1})),
    xl_memdb:load(testmemdb2, []),
    ?assertEquals(undefined, xl_memdb:get(testmemdb2, {a, 1})),
    ?assertEquals(undefined, xl_memdb:get(testmemdb2, {b, 1})),
    xl_memdb:load(testmemdb2, [{my_key, my_value}, {{other, key}, "Boo!"}]),
    ?assertEquals({ok, {my_key, my_value}}, xl_memdb:get(testmemdb2, my_key)),
    ?assertEquals({ok, {{other, key}, "Boo!"}}, xl_memdb:get(testmemdb2, {other, key})),
    xl_memdb:stop(testmemdb2).
