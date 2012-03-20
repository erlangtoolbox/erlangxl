-module(strikead_json_tests).

-include_lib("eunit/include/eunit.hrl").

-include("rec.hrl").

hrl_generation_test() ->
    R = #rec{req_int=2},
    erlang:display(R),
    erlang:display(rec:rec_to_json(R)),
    R2 = #rec2{opt_rec_def = R, opt_rec = R, opt_list_rec=[R,R]},
    erlang:display(R2),
    erlang:display(rec:rec2_to_json(R2)).
