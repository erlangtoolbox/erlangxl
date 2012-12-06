-module(xl_random).

-author("Dmitry Kasimtsev <dmitry.kasimtsev@strikead.com>").

-export([uniform/1]).

uniform(N) when is_integer(N) ->
    (xl_calendar:now_millis() rem N)+1.
