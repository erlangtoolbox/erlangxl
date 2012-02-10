-module(strikead_io).
-export([lines/1, parse_lines/1, posix_error/1, posix_error/2, apply_io/3]).

lines(IoDevice) -> strikead_stream:map(fun({L, _}) -> L end, parse_lines(IoDevice)).

parse_lines(IoDevice) ->
	strikead_stream:stream(IoDevice, fun(_) ->
		{ok, Pos} = file:position(IoDevice, {cur, 0}),
		case io:get_line(IoDevice, '') of
			eof -> empty;
			L -> {{L, Pos}, IoDevice}
		end
	end).

posix_error({error, Code}) -> {error, Code, erl_posix_msg:message(Code), notarget}.

posix_error(E = {error, Code}, Target) ->
	case is_posix_error(E) of
		true -> {error, Code, erl_posix_msg:message(Code), Target};
		_ -> {error, Code, Code, Target}
	end;
posix_error(E, Target) -> {error, E, Target}.

is_posix_error({error, Code}) -> erl_posix_msg:message(Code) /= "unknown POSIX error".

apply_io(Module, Func, Args) ->
	case apply(Module, Func, Args) of
		E = {error, _} -> posix_error(E, Args);
		X -> X
	end.
