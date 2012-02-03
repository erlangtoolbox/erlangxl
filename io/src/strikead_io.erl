-module(strikead_io).
-export([lines/1, parse_lines/1]).

lines(IoDevice) -> strikead_stream:map(fun({L, _}) -> L end, parse_lines(IoDevice)).

parse_lines(IoDevice) ->
	strikead_stream:stream(IoDevice, fun(_) ->
		{ok, Pos} = file:position(IoDevice, {cur, 0}),
		case io:get_line(IoDevice, '') of
			eof -> empty;
			L -> {{L, Pos}, IoDevice}
		end
	end).
