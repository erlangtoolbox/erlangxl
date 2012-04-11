-module(strikead_string_tests).

-include_lib("eunit/include/eunit.hrl").

strip_test() ->
    ?assertEqual("a b\tc", strikead_string:strip(" \ta b\tc \r\n")).

strip_empty_test() ->
    ?assertEqual("", strikead_string:strip("")).

stripthru_test() ->
    ?assertEqual("abc\"\\n\"", strikead_string:stripthru("a\tb\nc\"\\n\"")).

to_float_test() ->
	?assertEqual(0.0, strikead_string:to_float("0")),
	?assertEqual(0.0, strikead_string:to_float("0.0")).

substitute_test() ->
	?assertEqual("xyz1",
		strikead_string:substitute("x{a_a}z{b-b}", [{a_a, "y"}, {'b-b', 1}])),
	?assertEqual("xyz1",
		strikead_string:substitute("x{a}z{b}", [{a, "y"}, {b, 1}])),
	?assertEqual("xyzy",
		strikead_string:substitute("x{a}z{a}", [{a, "y"}])),
	?assertEqual("xyz{b+}",
		strikead_string:substitute("x{a}z{b+}", [{a, "y"}])),
	?assertEqual("xyz",
		strikead_string:substitute("x{a}z{b}", [{a, "y"}])),
	?assertEqual("xy",
		strikead_string:substitute("x{a.b}", [{'a.b', "y"}])),
	?assertEqual("xyz{}",
		strikead_string:substitute("x{a}z{}", [{a, "y"}])).

to_string_test() ->
	?assertEqual("1", strikead_string:to_string(1)),
	?assertEqual("2.2", strikead_string:to_string(2.2)),
	?assertEqual("true", strikead_string:to_string(true)),
	?assertEqual("x", strikead_string:to_string("x")),
	?assertEqual("Y", strikead_string:to_string(<<"Y">>)).
