%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(xl_calendar_tests).

-include_lib("eunit/include/eunit.hrl").
-include("xl_calendar.hrl").

add_test() ->
    ?assertEqual({{2013, 2, 28}, {0, 0, 0}},
                 xl_calendar:add({{2012, 2, 29}, {0, 0, 0}}, 1, years)).

day_of_week_test() ->
    ?assertEqual(xl_calendar:day_of_week(1383912159000), 'Fri'),
    ?assertEqual(xl_calendar:day_of_week({{2013, 11, 8}, {0, 0, 0}}), 'Fri').

format_test() ->
    ?assertEqual("Sun, 01-Feb-1970 00:00:01 GMT", xl_calendar:format("EEE, dd-MMM-yyyy HH:mm:ss GMT", {{1970, 2, 1}, {0, 0, 1}})),
    ?assertEqual("1970-02-01 00:00:01", xl_calendar:format("yyyy-MM-dd HH:mm:ss", {{1970, 2, 1}, {0, 0, 1}})),
    ?assertEqual(undefined, xl_calendar:format("yyyy-MM-dd HH:mm:ss", undefined)),
    ?assertEqual(<<"1970-02-01 00-00-01">>, xl_calendar:format("yyyy-MM-dd HH-mm-ss", {{1970, 2, 1}, {0, 0, 1}}, binary)),
    ?assertEqual(undefined, xl_calendar:format("yyyy-MM-dd HH-mm-ss", undefined, binary)).

format_perf_test() ->
    xl_eunit:performance(format, fun() ->
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


diff_periods_at_the_intersection_of_two_days_test() ->
    StartDate = {2013, 10, 1}, 
    FinishDate = {2013, 10, 2},
    DayStartTime = {0, 0, 0},
    DayFinishTime = {23, 59, 59},

    ?assertEqual(48 * 2, xl_calendar:diff_periods({StartDate, DayStartTime}, 
            {FinishDate, DayFinishTime}, xl_calendar:weekdays(), 
            xl_calendar:whole_day(), 30*?MS_IN_MIN)),

    xl_eunit:format("~n first test is passed ~n~n", []),

    ?assertEqual(48, xl_calendar:diff_periods({StartDate, DayFinishTime}, 
            {FinishDate, DayFinishTime}, xl_calendar:weekdays(), 
            xl_calendar:whole_day(), 30*?MS_IN_MIN)),
    
    xl_eunit:format("~n second test is passed ~n~n", []),

    ?assertEqual(48, xl_calendar:diff_periods({StartDate, DayStartTime}, 
            {FinishDate, DayStartTime}, xl_calendar:weekdays(), 
            xl_calendar:whole_day(), 30*?MS_IN_MIN)),

    xl_eunit:format("~n third test is passed ~n~n", []),

    ?assertEqual(1, xl_calendar:diff_periods({StartDate, DayFinishTime}, 
            {FinishDate, DayStartTime}, xl_calendar:weekdays(), 
            xl_calendar:whole_day(), 30*?MS_IN_MIN)),

    xl_eunit:format("~n fourth test is passed ~n~n", []),
    
    ?assertEqual(18 * 4, xl_calendar:diff_periods({StartDate, {6, 0, 0}},
            {FinishDate, {18, 0, 0}}, xl_calendar:weekdays(),
            xl_calendar:whole_day(), 30*?MS_IN_MIN)),

    xl_eunit:format("~n fifth test is passed ~n~n", []),
    
    %%  TODO: fix that + 1
    ?assertEqual(12 * 4 + 1, xl_calendar:diff_periods({StartDate, {18, 0, 0}},
            {FinishDate, {6, 0, 0}}, xl_calendar:weekdays(),
            xl_calendar:whole_day(), 30*?MS_IN_MIN)).

diff_periods_at_intersection_of_periods_test() ->
    Date = {2013, 10, 1},
    ?assertEqual(3, xl_calendar:diff_periods({Date, {15, 59, 59}}, {Date, {17, 30, 0}},
            xl_calendar:weekdays(), xl_calendar:whole_day(), 30*?MS_IN_MIN)),
    %% TODO: fix that - 1
    ?assertEqual(2 - 1, xl_calendar:diff_periods({Date, {16, 29, 59}}, {Date, {17, 0, 0}},
            xl_calendar:weekdays(), xl_calendar:whole_day(), 30*?MS_IN_MIN)),
    ?assertEqual(1, xl_calendar:diff_periods({Date, {16, 30, 0}}, {Date, {17, 0, 0}},
            xl_calendar:weekdays(), xl_calendar:whole_day(), 30*?MS_IN_MIN)),
    ?assertEqual(1 - 1, xl_calendar:diff_periods({Date, {16, 59, 59}}, {Date, {17, 0, 0}},
            xl_calendar:weekdays(), xl_calendar:whole_day(), 30*?MS_IN_MIN)).

diff_periods_different_periods_test() ->
    Start = {{2013, 10, 1}, {0, 0, 0}},
    Finish = {{2013, 10, 3}, {0, 0, 0}},
    ?assertEqual(1, xl_calendar:diff_periods(Start, Finish,
            xl_calendar:weekdays(), xl_calendar:whole_day(), ?MS_IN_DAY*2)),
    ?assertEqual(2, xl_calendar:diff_periods(Start, Finish,
            xl_calendar:weekdays(), xl_calendar:whole_day(), ?MS_IN_DAY)),
    ?assertEqual(48, xl_calendar:diff_periods(Start, Finish,
            xl_calendar:weekdays(), xl_calendar:whole_day(), ?MS_IN_HOUR)),
    ?assertEqual(48*2, xl_calendar:diff_periods(Start, Finish,
            xl_calendar:weekdays(), xl_calendar:whole_day(), 30*?MS_IN_MIN)),
    ?assertEqual(48*6, xl_calendar:diff_periods(Start, Finish,
            xl_calendar:weekdays(), xl_calendar:whole_day(), 10*?MS_IN_MIN)),
    ?assertEqual(48*12, xl_calendar:diff_periods(Start, Finish,
            xl_calendar:weekdays(), xl_calendar:whole_day(), 5*?MS_IN_MIN)),
    FinishInAWeek = {{2013, 10, 7}, {23,59,59}},
    ?assertEqual(1, xl_calendar:diff_periods(Start, FinishInAWeek,
            xl_calendar:weekdays(), xl_calendar:whole_day(), ?MS_IN_WEEK)),
    ?assertEqual(7, xl_calendar:diff_periods(Start, FinishInAWeek,
            xl_calendar:weekdays(), xl_calendar:whole_day(), ?MS_IN_DAY)).


diff_periods_test() ->
    ?assertEqual(31, xl_calendar:diff_periods({{2013, 10, 1},{1,45,0}},{{2013, 10, 1},{17, 00,0}},['Mon', 'Tue', 'Wed',
                'Thu', 'Fri', 'Sat', 'Sun'], xl_calendar:whole_day(), 30*60*1000)),
    ?assertEqual(31, xl_calendar:diff_periods({{2013, 10, 1},{1,50,0}},{{2013, 10, 1},{17, 00,0}},['Mon', 'Tue', 'Wed',
                'Thu', 'Fri', 'Sat', 'Sun'], xl_calendar:whole_day(), 30*60*1000)),
    ?assertEqual(3, xl_calendar:diff_periods({{2013, 10, 1},{0,0,0}},{{2013, 10, 1},{0, 30,0}},['Mon', 'Tue', 'Wed',
                'Thu', 'Fri', 'Sat', 'Sun'], xl_calendar:whole_day(), 10*60*1000)),
    ?assertEqual(3, xl_calendar:diff_periods({{2013, 10, 1},{0,0,0}},{{2013, 10, 1},{1, 30,0}},['Mon', 'Tue', 'Wed',
                'Thu', 'Fri', 'Sat', 'Sun'], xl_calendar:whole_day(), 30*60*1000)).

diff_hours_test() ->
    ?assertEqual(24 * 2, xl_calendar:diff_hours({{2013, 2, 23}, {0, 0, 0}}, {{2013, 2, 28}, {0, 0, 0}}, ['Mon', 'Wed'], xl_calendar:whole_day())),
    ?assertEqual(3 * 2, xl_calendar:diff_hours({{2013, 2, 23}, {6, 0, 0}}, {{2013, 2, 28}, {8, 0, 0}}, ['Mon', 'Wed'], [4, 7, 9])),
    ?assertEqual(3 + 2 + 2, xl_calendar:diff_hours({{2013, 2, 23}, {6, 0, 0}}, {{2013, 2, 27}, {8, 0, 0}}, ['Sat', 'Mon', 'Wed'], [4, 7, 9])),
    ?assertEqual(1, xl_calendar:diff_hours({{2013, 2, 23}, {6, 0, 0}}, {{2013, 2, 23}, {8, 0, 0}}, ['Sat', 'Mon', 'Wed'], [4, 7, 9])),
    ?assertEqual(0, xl_calendar:diff_hours({{2013, 2, 23}, {7, 0, 0}}, {{2013, 2, 23}, {7, 0, 0}}, ['Sat', 'Mon', 'Wed'], [4, 7, 9])).

diff_universal_days_test() ->
    %%  TODO: fix problem with reducer in xl_calendar:hours_count
    ?assertEqual(6, xl_calendar:diff_periods({{2013, 2, 1}, {0, 0, 0}}, {{2013, 2, 20}, {23, 59, 59}}, 
            ['Mon', 'Wed'], xl_calendar:whole_day(), ?MS_IN_DAY)),
    ?assertEqual(1, xl_calendar:diff_periods({{2013, 2, 12}, {0, 0, 0}}, {{2013, 2, 15}, {23, 59, 59}}, 
            ['Mon', 'Wed'], xl_calendar:whole_day(), ?MS_IN_DAY)),
    ?assertEqual(2, xl_calendar:diff_periods({{2013, 2, 12}, {0, 0, 0}}, {{2013, 2, 18}, {23, 59, 59}}, 
            ['Mon', 'Wed'], xl_calendar:whole_day(), ?MS_IN_DAY)),
    ?assertEqual(1, xl_calendar:diff_periods({{2013, 2, 23}, {0, 0, 0}}, {{2013, 2, 23}, {23, 59, 59}}, 
            ['Sat', 'Mon', 'Wed'], xl_calendar:whole_day(), ?MS_IN_DAY)).

diff_days_test() ->
    ?assertEqual(6, xl_calendar:diff_days({{2013, 2, 1}, {0, 0, 0}}, {{2013, 2, 20}, {0, 0, 0}}, ['Mon', 'Wed'])),
    ?assertEqual(1, xl_calendar:diff_days({{2013, 2, 12}, {0, 0, 0}}, {{2013, 2, 15}, {0, 0, 0}}, ['Mon', 'Wed'])),
    ?assertEqual(2, xl_calendar:diff_days({{2013, 2, 12}, {0, 0, 0}}, {{2013, 2, 18}, {0, 0, 0}}, ['Mon', 'Wed'])),
    ?assertEqual(1, xl_calendar:diff_days({{2013, 2, 23}, {6, 0, 0}}, {{2013, 2, 23}, {8, 0, 0}}, ['Sat', 'Mon', 'Wed'])).



weekdays_member_test() ->
    ?assert(xl_calendar:weekdays_member('Tue', ['Mon', 'Tue'])),
    ?assertNot(xl_calendar:weekdays_member('Tue', ['Mon', 'Wed'])),
    Week = xl_calendar:weekdays(),
    Mask = xl_calendar:weekdays_mask(Week),
    xl_eunit:performance(weekdays_member, fun() ->
        xl_calendar:weekdays_member('Thu', Mask)
    end, 10000),
    xl_eunit:performance(lists_member, fun() ->
        lists:member('Thu', Week)
    end, 10000).

number_of_days_test() ->
    ?assertEqual(11, xl_calendar:number_of_days(
            ['Mon', 'Tue', 'Wed'],
            xl_calendar:datetime_to_ms({{2013, 10, 1}, {0,0,0}}),
            xl_calendar:datetime_to_ms({{2013, 10, 23}, {0,0,0}})
        )).

