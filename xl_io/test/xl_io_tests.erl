-module(xl_io_tests).

-include_lib("eunit/include/eunit.hrl").

lines_test() ->
    {ok, ["123\n", "456\n", "789"]} = xl_file:using(xl_eunit:resource(?MODULE, "lines.txt"), [read], fun(F) ->
        xl_stream:to_list(xl_io:lines(F))
    end).

parse_lines_test() ->
    {ok, [{"123\n", 0}, {"456\n", 4}, {"789", 8}]} = xl_file:using(xl_eunit:resource(?MODULE, "lines.txt"), [read], fun(F) ->
        xl_stream:to_list(xl_io:parse_lines(F))
    end).

