-module(xl_string_tests).

-include_lib("eunit/include/eunit.hrl").
-include("xl_lang.hrl").

strip_test() ->
    ?assertEqual("a b\tc", xl_string:strip(" \ta b\tc \r\n")).

strip_empty_test() ->
    ?assertEqual("", xl_string:strip("")).

stripthru_test() ->
    ?assertEqual("abc\"\\n\"", xl_string:stripthru("a\tb\nc\"\\n\"")).

substitute_test() ->
    ?assertEqual(<<"xyz1">>,
        xl_string:substitute(<<"x@a_a@z@b-b@">>, [{a_a, "y"}, {'b-b', 1}], {$@, $@})),
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

join_performance_test() ->
    MixedStrings = ["a", 1, "b", 2.3, c, "a", 1, "b", 2.3, c, "a", 1, "b", 2.3, c],
    MixedBinaries = [<<"a">>, 1, <<"b">>, 2.3, c, <<"a">>, 1, <<"b">>, 2.3, c, <<"a">>, 1, <<"b">>, 2.3, c],
    Strings = ["a", "b", "a", "b", "a", "b"],
    Binaries = [<<"a">>, <<"b">>, <<"a">>, <<"b">>, <<"a">>, <<"b">>],
    MixedStringsXps = xl_eunit:performance("mixed strings", fun(_) ->
        xl_string:join(MixedStrings, "")
    end, 10000),
    MixedBinariesXps = xl_eunit:performance("mixed binaries", fun(_) ->
        xl_string:join(MixedBinaries, <<"">>)
    end, 10000),
    StringsXps = xl_eunit:performance("strings", fun(_) ->
        xl_string:join(Strings, "")
    end, 10000),
    BinariesXps = xl_eunit:performance("binaries", fun(_) ->
        xl_string:join(Binaries, <<"">>)
    end, 10000),
    ?assert(MixedBinariesXps > MixedStringsXps),
    ?assert(BinariesXps < StringsXps).

unquote_test() ->
    ?assertEqual("aaa\"a", xl_string:unquote("\"aaa\\\"a\"")).

replace_test() ->
    ?assertEqual("abeabe", xl_string:replace("abcdeabcde", "cd", "")).

-record(r, {a, b, c}).
format_record_test() ->
    R = #r{a = 1, b = 2, c = c},
    ?assertEqual("#r{a = 1, b = 2, c = c}", ?FORMAT_RECORD(R, r)).
