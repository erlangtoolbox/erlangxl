-module(strikead_io_tests).

-include_lib("eunit/include/eunit.hrl").

lines_test() ->
        {ok, ["123\n", "456\n", "789"]} = strikead_autofile:using(strikead_eunit:resource(?MODULE, "lines.txt"), [read], fun(F) ->
			strikead_stream:to_list(strikead_io:lines(F))
		end).

parse_lines_test() ->
        {ok, [{"123\n",0}, {"456\n",4}, {"789",8}]} = strikead_autofile:using(strikead_eunit:resource(?MODULE, "lines.txt"), [read], fun(F) ->
			strikead_stream:to_list(strikead_io:parse_lines(F))
		end).


