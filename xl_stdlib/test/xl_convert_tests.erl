-module(xl_convert_tests).

-include_lib("eunit/include/eunit.hrl").

to_float_test() ->
    ?assertEqual(0.0, xl_convert:to_float("0")),
    ?assertEqual(0.0, xl_convert:to_float("0.0")).

to_string_test() ->
    ?assertEqual("1", xl_convert:to_string(1)),
    ?assertEqual("2.2", xl_convert:to_string(2.2)),
    ?assertEqual("true", xl_convert:to_string(true)),
    ?assertEqual("x", xl_convert:to_string("x")),
    ?assertEqual("Y", xl_convert:to_string(<<"Y">>)).

to_atom_test() ->
    ?assertEqual('AB', xl_convert:to_atom("AB")).

to_binary_test() ->
    ?assertEqual(<<"1">>, xl_convert:to_binary(1)),
    ?assertEqual(<<"str">>, xl_convert:to_binary("str")).
