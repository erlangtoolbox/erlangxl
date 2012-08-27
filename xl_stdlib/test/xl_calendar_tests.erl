-module(xl_calendar_tests).

-include_lib("eunit/include/eunit.hrl").

add_test() ->
    ?assertEqual({{2013, 2, 28}, {0, 0, 0}}, xl_calendar:add({{2012, 2, 29}, {0, 0, 0}}, 1, years)).

format_test() ->
    ?assertEqual("Sun, 01-Feb-1970 00:00:01 GMT", xl_calendar:format("EEE, dd-MMM-yyyy HH:mm:ss GMT", {{1970, 2, 1}, {0, 0, 1}})),
    ?assertEqual("1970-02-01 00:00:01", xl_calendar:format("yyyy-MM-dd HH:mm:ss", {{1970, 2, 1}, {0, 0, 1}})).

