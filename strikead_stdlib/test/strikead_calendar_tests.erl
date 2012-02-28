-module(strikead_calendar_tests).

-include_lib("eunit/include/eunit.hrl").

advance_test() ->
    ?assertEqual({{2011,12,10},{0,0,0}}, strikead_calendar:advance({{2011,10,10}, {0,0,0}}, {{0,2,0},{0,0,0}})),
    ?assertEqual({{2011,10,24},{0,0,0}}, strikead_calendar:advance({{2011,10,10}, {0,0,0}}, {{0,0,14},{0,0,0}})),
    ?assertEqual({{2011,10,30},{0,0,0}}, strikead_calendar:advance({{2011,10,20}, {0,0,0}}, {{0,0,10},{0,0,0}})),
    ?assertEqual({{2011,11,3},{0,0,0}}, strikead_calendar:advance({{2011,10,20}, {0,0,0}}, {{0,0,14},{0,0,0}})),
    ?assertEqual({{2012,1,3},{0,0,0}}, strikead_calendar:advance({{2011,12,20}, {0,0,0}}, {{0,0,14},{0,0,0}})),
    ?assertEqual({{2011,12,31},{0,0,0}}, strikead_calendar:advance({{2011,12,20}, {0,0,0}}, {{0,0,11},{0,0,0}})),
    ?assertEqual({{2012,1,1},{0,0,0}}, strikead_calendar:advance({{2011,12,20}, {0,0,0}}, {{0,0,12},{0,0,0}})),
    ?assertEqual({{2014,1,1},{0,0,0}}, strikead_calendar:advance({{2011,12,20}, {0,0,0}}, {{1,12,12},{0,0,0}})),
    ?assertEqual({{2011,12,21},{5,48,17}}, strikead_calendar:advance({{2011,12,20}, {23,44,11}}, {{0,0,0},{6,4,6}})),
    ?assertEqual({{2011,12,1},{0,0,0}}, strikead_calendar:advance({{2011,11,17},{0,0,0}}, {{0,0,14},{0,0,0}})).

format_test() ->
    ?assertEqual("Sun, 01-Feb-1970 00:00:01 GMT", strikead_calendar:format("EEE, dd-MMM-yyyy HH:mm:ss GMT", {{1970,2,1},{0,0,1}})),
    ?assertEqual("1970-02-01 00:00:01", strikead_calendar:format("yyyy-MM-dd HH:mm:ss", {{1970,2,1},{0,0,1}})).
