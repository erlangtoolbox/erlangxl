-module(strikead_eunit).

-include_lib("eunit/include/eunit.hrl").

-export([resource/2]).

resource(Module, Name) ->
	case code:which(Module) of
		E = non_existing -> throw({E, Module});
		X -> filename:dirname(X) ++ "/../test/" ++ Name
	end.

