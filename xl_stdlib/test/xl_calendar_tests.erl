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

hourly_test() ->
    Raw =
        [{{{2013, 2, 23}, {20, 2, 3}}, {{2013, 2, 23}, {21, 0, 0}}},
            {{{2013, 2, 23}, {21, 0, 0}}, {{2013, 2, 23}, {22, 0, 0}}},
            {{{2013, 2, 23}, {22, 0, 0}}, {{2013, 2, 23}, {23, 0, 0}}},
            {{{2013, 2, 23}, {23, 0, 0}}, {{2013, 2, 24}, {0, 0, 0}}},
            {{{2013, 2, 24}, {0, 0, 0}}, {{2013, 2, 24}, {1, 0, 0}}},
            {{{2013, 2, 24}, {1, 0, 0}}, {{2013, 2, 24}, {2, 0, 0}}},
            {{{2013, 2, 24}, {2, 0, 0}}, {{2013, 2, 24}, {2, 3, 4}}}],
    RawSeconds = (3600 - (2 * 60 + 3)) + 5 * 3600 + (3 * 60 + 4),
    FilterDays =
        [{{{2013, 2, 24}, {0, 0, 0}}, {{2013, 2, 24}, {1, 0, 0}}},
            {{{2013, 2, 24}, {1, 0, 0}}, {{2013, 2, 24}, {2, 0, 0}}},
            {{{2013, 2, 24}, {2, 0, 0}}, {{2013, 2, 24}, {2, 3, 4}}}],
    FilterHours =
        [{{{2013, 2, 23}, {20, 2, 3}}, {{2013, 2, 23}, {21, 0, 0}}},
            {{{2013, 2, 23}, {21, 0, 0}}, {{2013, 2, 23}, {22, 0, 0}}},
            {{{2013, 2, 23}, {23, 0, 0}}, {{2013, 2, 24}, {0, 0, 0}}},
            {{{2013, 2, 24}, {0, 0, 0}}, {{2013, 2, 24}, {1, 0, 0}}},
            {{{2013, 2, 24}, {1, 0, 0}}, {{2013, 2, 24}, {2, 0, 0}}}],
    Schedule = xl_calendar:hourly({{2013, 2, 23}, {20, 2, 3}},
        {{2013, 2, 24}, {2, 3, 4}}),
    ?assertEqual(Raw, Schedule),
    ?assertEqual(FilterDays, xl_calendar:filter_weekdays(Schedule, ['Sat'])),
    ?assertEqual(FilterHours, xl_calendar:filter_hours(Schedule, [2, 22])),
    ?assertEqual(RawSeconds, xl_calendar:seconds_hourly(Schedule)).


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

