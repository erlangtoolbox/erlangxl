-module(strikead_json_binder_tests).

-include_lib("eunit/include/eunit.hrl").

-include("rec.hrl").

generic_test() ->
    R = #rec{},
    ?assertEqual(R, rec:from_json(rec:to_json(R), rec)),

    R2 = #rec2{opt_rec_def = R, opt_rec = R, opt_list_rec=[R,R]},
    erlang:display(rec:to_json(R2)),
    ?assertEqual(R2, rec:from_json(rec:to_json(R2), rec2)).

undefined_null_test() ->
    ?assertEqual("{\"a\": null}", rec:to_json(#simple{})),
    ?assertEqual("{\"a\": null}", rec:to_json(#nullobj{})),
    ?assertEqual("{\"a\": null}", rec:to_json(#strnull{})),
    ?assertEqual("{\"a\": \"a\\nb\"}", rec:to_json(#strnull{a="a\nb"})),
    ?assertEqual("{\"a\": \"\"}", rec:to_json(#strnull{a=""})),
    ?assertEqual(#simple{}, rec:from_json(rec:to_json(#simple{}),simple)),
    ?assertEqual(#nullobj{}, rec:from_json(rec:to_json(#nullobj{}),nullobj)).


tuple_test() ->
    ?assertEqual("{\"a\": {\"x\": 1, \"y\": 2}}", rec:to_json(#tupleobj{a={{x,1}, {y,2}}})),
    ?assertEqual("{\"a\": {\"x\": \"\", \"y\": 2}}", rec:to_json(#tupleobj{a={{x,""}, {y,2}}})),
    ?assertEqual("{\"a\": {\"x\": {\"z\": [\"a\",\"b\"]}, \"y\": 2}}", rec:to_json(#tupleobj{a={{x,{{z,["a", "b"]}}}, {y,2}}})).


