%% Copyright
-module(xl_scheduler_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

-export([timed/0]).

schedule_test() ->
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



