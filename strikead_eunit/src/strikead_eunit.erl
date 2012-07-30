-module(strikead_eunit).

-include_lib("eunit/include/eunit.hrl").

-export([resource/2, explode/3]).

resource(Module, Name) -> within(Module, fun(Path) -> filename:join(Path, Name) end).

explode(Module, Name, Target) ->
    within(Module, fun(Path) ->
        strikead_file:copy(filename:join(Path, Name), Target)
    end).


within(Module, Fun) ->
    case code:which(Module) of
        non_existing -> {error, {non_existing, Module}};
        X -> Fun(filename:dirname(X) ++ "/../test")
    end.

