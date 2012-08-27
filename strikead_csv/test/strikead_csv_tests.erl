-module(strikead_csv_tests).

-include_lib("eunit/include/eunit.hrl").

parse_line_test() ->
    ["12", "23", "34"] = strikead_csv:parse_line("\"12\",\"23\",\"34\"").

parse_line_whitespaces_test() ->
    ["1 2", "2\t3", "3,4"] = strikead_csv:parse_line("   \"1 2\" , \"2\t3\" , \"3,4\" \r\n").

parse_line_empty_test() ->
    ["", "23", ""] = strikead_csv:parse_line("\"\",\"23\",\"\"").

parse_line_quoted_test() ->
    ["1", "2\"x\"3", "2"] = strikead_csv:parse_line("\"1\",\"2\"\"x\"\"3\",\"2\"").
