%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @copyright (C) 2014, strikead.com
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xl_ets_tests).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

transform_test() ->
    ETS = ets:new(tranform, [ordered_set]),
    ets:insert(ETS, [{a, 1}, {b, 2}]),
    xl_ets:transform(ETS, fun({Key, X}) -> {Key, X, X + 2} end),
    ?assertEqual([{a, 1, 3}, {b, 2, 4}], ets:tab2list(ETS)).
