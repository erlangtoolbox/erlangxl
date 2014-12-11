%% Copyright
-module(xl_scheduler_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

-export([timed/0, stated/1]).

interval_test() ->
    xl_application:start(xl_stdlib),
    xl_state:new(?MODULE),
    xl_state:set(?MODULE, count, 0),
    xl_scheduler:interval(counter, 100, ?MODULE, timed, []),
    timer:sleep(500),
    xl_scheduler:cancel(counter),
    timer:sleep(500),
    ?assertEqual({ok, 1}, xl_state:value(?MODULE, count)).


timed() ->
    xl_state:increment(?MODULE, count),
    timer:sleep(1000).

interval_with_state_test() ->
    xl_application:start(xl_stdlib),
    xl_state:new(?MODULE),
    xl_state:set(?MODULE, state, 0),
    xl_scheduler:interval_with_state(counter, 100, ?MODULE, stated, [], 0),
    timer:sleep(550),
    xl_scheduler:cancel(counter),
    timer:sleep(50),
    ?assertEqual({ok, 4}, xl_state:value(?MODULE, state)).

stated(C) ->
    xl_state:set(?MODULE, state, C),
    C + 1.



