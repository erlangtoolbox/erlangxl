-module(xl_eunit).

-include_lib("eunit/include/eunit.hrl").

-export([resource/2, explode/3, performance/3, format/2, format/1]).

resource(Module, Name) -> within(Module, fun(Path) -> filename:join(Path, Name) end).

explode(Module, Name, Target) ->
    within(Module, fun(Path) ->
        xl_file:copy(filename:join(Path, Name), Target)
    end).


within(Module, Fun) ->
    case code:which(Module) of
        non_existing -> {error, {non_existing, Module}};
        X -> Fun(filename:dirname(X))
    end.

performance(Name, Fun, Count) ->
    Times = lists:seq(0, Count),
    {Time, _} = timer:tc(fun() ->
        lists:foreach(Fun, Times)
    end),
    Xps = Count / Time * 1000000,
    xl_eunit:format("PERFORMANCE ~s: ~.1f op/s~n", [Name, Count / Time * 1000000]),
    Xps.

-spec(format(string(), [term()]) -> ok).
format(Format, Args) -> io:format(user, Format, Args).

-spec(format(string()) -> ok).
format(Format) -> format(Format, []).
