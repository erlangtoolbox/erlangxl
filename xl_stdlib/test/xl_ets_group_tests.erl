%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @copyright (C) 2014, strikead.com
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xl_ets_group_tests).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-include("xl_eunit.hrl").

all_test() ->
    xl_application:start(xl_stdlib),
    ?assertOk(xl_ets_group:start(etsg1)),
    Ets1 = xl_ets_group:new_ets(etsg1, ets1, [named_table, public, set]),
    Ets2 = xl_ets_group:new_ets(etsg1, ets2, [named_table, public, set]),
    ?assertEquals({ok, Ets1}, xl_ets_group:get_ets(etsg1, ets1)),
    ?assertEquals({ok, Ets2}, xl_ets_group:get_ets(etsg1, ets2)),
    ?assertOk(xl_ets_group:clear(etsg1)),
    ?assertEquals(undefined, xl_ets_group:get_ets(etsg1, ets1)),
    ?assertEquals(undefined, xl_ets_group:get_ets(etsg1, ets2)),
    ?assertEquals(undefined, ets:info(ets1)),
    ?assertEquals(undefined, ets:info(ets2)),
    xl_ets_group:new_ets(etsg1, ets3, [named_table, public, set]),
    xl_ets_group:stop(etsg1),
    ?assertEquals(undefined, ets:info(ets3)).
