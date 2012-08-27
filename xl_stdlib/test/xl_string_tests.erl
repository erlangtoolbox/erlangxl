-module(xl_string_tests).

-include_lib("eunit/include/eunit.hrl").

strip_test() ->
    ?assertEqual("a b\tc", xl_string:strip(" \ta b\tc \r\n")).

strip_empty_test() ->
    ?assertEqual("", xl_string:strip("")).

stripthru_test() ->
    ?assertEqual("abc\"\\n\"", xl_string:stripthru("a\tb\nc\"\\n\"")).

to_float_test() ->
    ?assertEqual(0.0, xl_string:to_float("0")),
    ?assertEqual(0.0, xl_string:to_float("0.0")).

substitute_test() ->
    ?assertEqual("xyz1",
        xl_string:substitute("x{a_a}z{b-b}", [{a_a, "y"}, {'b-b', 1}])),
    ?assertEqual("xyz1",
        xl_string:substitute("x{a}z{b}", [{a, "y"}, {b, 1}])),
    ?assertEqual("xyzy",
        xl_string:substitute("x{a}z{a}", [{a, "y"}])),
    ?assertEqual("xyz{b+}",
        xl_string:substitute("x{a}z{b+}", [{a, "y"}])),
    ?assertEqual("xyz",
        xl_string:substitute("x{a}z{b}", [{a, "y"}])),
    ?assertEqual("xy",
        xl_string:substitute("x{a.b}", [{'a.b', "y"}])),
    ?assertEqual("xyz{}",
        xl_string:substitute("x{a}z{}", [{a, "y"}])).

to_string_test() ->
    ?assertEqual("1", xl_string:to_string(1)),
    ?assertEqual("2.2", xl_string:to_string(2.2)),
    ?assertEqual("true", xl_string:to_string(true)),
    ?assertEqual("x", xl_string:to_string("x")),
    ?assertEqual("Y", xl_string:to_string(<<"Y">>)).

mk_atom_test() ->
    ?assertEqual(atomAB1, xl_string:mk_atom([atom, "AB", 1])).

equal_ignore_case_test() ->
    ?assert(xl_string:equal_ignore_case(<<"A">>, <<"a">>)),
    ?assert(xl_string:equal_ignore_case("A", "a")),
    ?assert(xl_string:equal_ignore_case(<<"A">>, "a")),
    ?assert(xl_string:equal_ignore_case("A", <<"a">>)).

join_test() ->
    ?assertEqual(<<"aaa;bbb;ccc">>,
        xl_string:join([<<"aaa">>, "bbb", <<"ccc">>], <<";">>)),
    ?assertEqual("aaa;bbb;ccc",
        xl_string:join([<<"aaa">>, "bbb", <<"ccc">>], ";")),
    ?assertEqual("aaabbbccc",
        xl_string:join([<<"aaa">>, "bbb", <<"ccc">>])).

