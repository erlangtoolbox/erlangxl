-module(xl_calendar_tests).

-include_lib("eunit/include/eunit.hrl").

add_test() ->
    ?assertEqual({{2013, 2, 28}, {0, 0, 0}}, xl_calendar:add({{2012, 2, 29}, {0, 0, 0}}, 1, years)).

format_test() ->
    ?assertEqual("Sun, 01-Feb-1970 00:00:01 GMT", xl_calendar:format("EEE, dd-MMM-yyyy HH:mm:ss GMT", {{1970, 2, 1}, {0, 0, 1}})),
    ?assertEqual("1970-02-01 00:00:01", xl_calendar:format("yyyy-MM-dd HH:mm:ss", {{1970, 2, 1}, {0, 0, 1}})),
    ?assertEqual(undefined, xl_calendar:format("yyyy-MM-dd HH:mm:ss", undefined)),
    ?assertEqual(<<"1970-02-01 00-00-01">>, xl_calendar:format("yyyy-MM-dd HH-mm-ss", {{1970, 2, 1}, {0, 0, 1}}, binary)),
    ?assertEqual(undefined, xl_calendar:format("yyyy-MM-dd HH-mm-ss", undefined, binary)).

format_perf_test() ->
    xl_eunit:performance(format, fun(_) ->
        xl_calendar:format("EEE, dd-MMM-yyyy HH:mm:ss GMT", {{1970, 2, 1}, {0, 0, 1}})
    end, 100000).

ms_to_datetime_test() ->
    ?assertEqual({{2012, 10, 17}, {12, 3, 48}}, xl_calendar:ms_to_datetime(1350475428378)).

datetime_to_ms_test() ->
    ?assertEqual(1350475428000, xl_calendar:datetime_to_ms({{2012, 10, 17}, {12, 3, 48}})).

weekdays_order_test() ->
    ?assertEqual(['Mon', 'Wed', 'Sun'], lists:sort(xl_calendar:weekdays_order(), ['Wed', 'Sun', 'Mon'])).

adjust_test() ->
    {S1, F1} = xl_calendar:adjust({{2013, 2, 23}, {0, 0, 0}}, {{2013, 2, 28}, {0, 0, 0}}, ['Mon', 'Wed'], xl_calendar:whole_day()),
    ?assertEqual('Mon', xl_calendar:day_of_week(S1)),
    ?assertEqual('Wed', xl_calendar:day_of_week(F1)),

    {S4, F4} = xl_calendar:adjust({{2013, 2, 23}, {0, 0, 0}}, {{2013, 2, 28}, {0, 0, 0}}, ['Mon'], xl_calendar:whole_day()),
    ?assertEqual('Mon', xl_calendar:day_of_week(S4)),
    ?assertEqual('Mon', xl_calendar:day_of_week(F4)),

    {S2, F2} = xl_calendar:adjust({{2013, 2, 23}, {0, 0, 0}}, {{2013, 2, 28}, {0, 0, 0}}, ['Fri'], xl_calendar:whole_day()),
    ?assertEqual({{2013, 2, 28}, {0, 0, 0}}, S2),
    ?assertEqual({{2013, 2, 28}, {0, 0, 0}}, F2),

    {S3, F3} = xl_calendar:adjust({{2013, 2, 23}, {0, 0, 0}}, {{2013, 2, 28}, {0, 0, 0}}, [], xl_calendar:whole_day()),
    ?assertEqual({{2013, 2, 28}, {0, 0, 0}}, S3),
    ?assertEqual({{2013, 2, 28}, {0, 0, 0}}, F3).

diff_hours_test() ->
    ?assertEqual(24 * 2, xl_calendar:diff_hours({{2013, 2, 23}, {0, 0, 0}}, {{2013, 2, 28}, {0, 0, 0}}, ['Mon', 'Wed'], xl_calendar:whole_day())),
    ?assertEqual(3 * 2, xl_calendar:diff_hours({{2013, 2, 23}, {6, 0, 0}}, {{2013, 2, 28}, {8, 0, 0}}, ['Mon', 'Wed'], [4, 7, 9])),
    ?assertEqual(3 + 2 + 2, xl_calendar:diff_hours({{2013, 2, 23}, {6, 0, 0}}, {{2013, 2, 27}, {8, 0, 0}}, ['Sat', 'Mon', 'Wed'], [4, 7, 9])),
    ?assertEqual(1, xl_calendar:diff_hours({{2013, 2, 23}, {6, 0, 0}}, {{2013, 2, 23}, {8, 0, 0}}, ['Sat', 'Mon', 'Wed'], [4, 7, 9])).

diff_days_test() ->
    ?assertEqual(6, xl_calendar:diff_days({{2013, 2, 1}, {0, 0, 0}}, {{2013, 2, 20}, {0, 0, 0}}, ['Mon', 'Wed'])),
    ?assertEqual(1, xl_calendar:diff_days({{2013, 2, 12}, {0, 0, 0}}, {{2013, 2, 15}, {0, 0, 0}}, ['Mon', 'Wed'])),
    ?assertEqual(2, xl_calendar:diff_days({{2013, 2, 12}, {0, 0, 0}}, {{2013, 2, 18}, {0, 0, 0}}, ['Mon', 'Wed'])),
    ?assertEqual(1, xl_calendar:diff_days({{2013, 2, 23}, {6, 0, 0}}, {{2013, 2, 23}, {8, 0, 0}}, ['Sat', 'Mon', 'Wed'])).


