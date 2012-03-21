-module(strikead_json_tests).

-include_lib("eunit/include/eunit.hrl").

-include("rec.hrl").

generic_test() ->
    R = #rec{req_int=2},
    ?assertEqual(R, rec:from_json(rec:to_json(R), rec)),

    R2 = #rec2{opt_rec_def = R, opt_rec = R, opt_list_rec=[R,R]},
    erlang:display(rec:to_json(R2)),
    ?assertEqual(R2, rec:from_json(rec:to_json(R2), rec2)).

undefined_null_test() ->
    ?assertEqual("{\"a\": null}", rec:to_json(#simple{})),
    ?assertEqual("{\"a\": null}", rec:to_json(#nullobj{})),
    ?assertEqual(#simple{}, rec:from_json(rec:to_json(#simple{}),simple)),
    ?assertEqual(#nullobj{}, rec:from_json(rec:to_json(#nullobj{}),nullobj)).


