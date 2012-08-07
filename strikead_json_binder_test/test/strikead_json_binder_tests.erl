-module(strikead_json_binder_tests).

-include_lib("eunit/include/eunit.hrl").

-include("rec.hrl").

generic_test() ->
    R = #rec{},
    ?assertEqual({ok, R}, rec:from_json(rec:to_json(R), rec)),

    R2 = #rec2{opt_rec_def = R, opt_rec = R, opt_list_rec = [R, R]},
    erlang:display(rec:to_json(R2)),
    ?assertEqual({ok, R2}, rec:from_json(rec:to_json(R2), rec2)).

optinal_values_test() ->
    ?assertEqual({ok, #rec{}}, rec:from_json("{}", rec)).

undefined_null_test() ->
    ?assertEqual("{\"a\":null}", rec:to_json(#simple{})),
    ?assertEqual("{\"a\":null}", rec:to_json(#nullobj{})),
    ?assertEqual("{\"a\":null}", rec:to_json(#strnull{})),
    ?assertEqual("{\"a\":\"a\\nb\"}", rec:to_json(#strnull{a = <<"a\nb">>})),
    ?assertEqual("{\"a\":\"\"}", rec:to_json(#strnull{a = <<>>})),
    ?assertEqual({ok, #simple{}}, rec:from_json(rec:to_json(#simple{}), simple)),
    ?assertEqual({ok, #nullobj{}}, rec:from_json(rec:to_json(#nullobj{}), nullobj)).


tuple_test() ->
    ?assertEqual("{\"a\":{\"x\":1,\"y\":2}}",
        rec:to_json(#tupleobj{a=[{x,1}, {y,2}]})),
    ?assertEqual("{\"a\":{\"x\":\"\",\"y\":2}}",
        rec:to_json(#tupleobj{a=[{x,<<>>}, {y,2}]})),
    ?assertEqual("{\"a\":{\"x\":{\"z\":[\"a\",\"b\"]},\"y\":2}}",
        rec:to_json(#tupleobj{a=[{x,[{z,[<<"a">>, <<"b">>]}]}, {y,2}]})).


optional_obj_lists_bug_test() ->
    ?assertEqual({ok, #insertion_order{
            id = <<"3">>,
            name = <<"campaign3">>,
            total_budget = <<"eppp">>,
            creatives = []}
        }, rec:from_json("{\"id\":\"3\",\"name\":\"campaign3\",\"total_budget\": \"eppp\"}", insertion_order)).
