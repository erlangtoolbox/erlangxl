-module(xl_csv_tests).

-include_lib("eunit/include/eunit.hrl").

parse_line_test() ->
    ["12", "23", "34"] = xl_csv:parse_line("\"12\",\"23\",\"34\"").

parse_line_whitespaces_test() ->
    ["1 2", "2\t3", "3,4"] = xl_csv:parse_line("   \"1 2\" , \"2\t3\" , \"3,4\" \r\n").

parse_line_empty_test() ->
    ["", "23", ""] = xl_csv:parse_line("\"\",\"23\",\"\"").

parse_line_quoted_test() ->
    ["1", "2\"x\"3", "2"] = xl_csv:parse_line("\"1\",\"2\"\"x\"\"3\",\"2\"").
