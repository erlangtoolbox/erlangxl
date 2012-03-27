-module(strikead_string_tests).

-include_lib("eunit/include/eunit.hrl").

strip_test() ->
    ?assertEqual("a b\tc", strikead_string:strip(" \ta b\tc \r\n")).

strip_empty_test() ->
    ?assertEqual("", strikead_string:strip("")).

stripthru_test() ->
    ?assertEqual("abc\"\\n\"", strikead_string:stripthru("a\tb\nc\"\\n\"")).
