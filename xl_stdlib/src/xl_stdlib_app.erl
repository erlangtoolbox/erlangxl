%% Copyright
-module(xl_stdlib_app).
-author("volodymyr.kyrychenko@strikead.com").

-behaviour(application).

% application
-export([start/2, stop/1]).

% application callbacks
start(_Type, _Args) ->
    xl_state:start_link(),
    xl_uid:start(),
    xl_stdlib_sup:start_link().

stop(_State) -> ok.
