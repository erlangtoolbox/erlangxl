-module(xl_calendar).

-export([format/3, format/2, now_millis/0, now_micros/0, add/3, ms_to_datetime/1,
    day_of_week/1, datetime_to_ms/1, weekdays/0, weekdays_order/0, adjust/4, whole_day/0, diff_hours/4, daynum_of_week/1, diff_days/3, daynum/1, dayname/1]).

% add code is borrowed from http://code.google.com/p/dateutils
% Copyright (c) 2009 Jonas Enlund
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.


-spec(add(calendar:datetime(), integer(), seconds | minutes | hours | days | weeks | months | years) -> calendar:datetime()).

-type(weekday() :: 'Mon' | 'Tue' | 'Wed' | 'Thu' | 'Fri' | 'Sat' | 'Sun').
-export_type([weekday/0]).

add(DateTime, N, seconds) ->
    T1 = calendar:datetime_to_gregorian_seconds(DateTime),
    T2 = T1 + N,
    calendar:gregorian_seconds_to_datetime(T2);

add(DateTime, N, minutes) -> add(DateTime, 60 * N, seconds);
add(DateTime, N, hours) -> add(DateTime, 60 * N, minutes);

add(DateTime, N, days) -> add(DateTime, 24 * N, hours);

add(DateTime, N, weeks) -> add(DateTime, 7 * N, days);

add({{YYYY, MM, DD} = Date, Time}, 0, months) ->
    case calendar:valid_date(Date) of
        true -> {Date, Time};
        false -> add({{YYYY, MM, DD - 1}, Time}, 0, months) % Rolling back illegal 31,29
    end;

add({{YYYY, MM, DD}, Time}, N, months) when N > 0 andalso MM < 12 -> add({{YYYY, MM + 1, DD}, Time}, N - 1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N > 0 andalso MM =:= 12 -> add({{YYYY + 1, 1, DD}, Time}, N - 1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N < 0 andalso MM > 1 -> add({{YYYY, MM - 1, DD}, Time}, N + 1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N < 0 andalso MM =:= 1 -> add({{YYYY - 1, 12, DD}, Time}, N + 1, months);
add(Date, N, years) -> add(Date, 12 * N, months).

% end of Jonas Enlund

day_of_week(Date) when is_integer(Date) -> day_of_week(ms_to_datetime(Date));
day_of_week({Date, _}) -> dayname(calendar:day_of_the_week(Date)).

daynum_of_week(Date) when is_integer(Date) -> daynum_of_week(ms_to_datetime(Date));
daynum_of_week({Date, _}) -> calendar:day_of_the_week(Date).

daynum(Day) ->
    case Day of
        'Mon' -> 1;
        'Tue' -> 2;
        'Wed' -> 3;
        'Thu' -> 4;
        'Fri' -> 5;
        'Sat' -> 6;
        'Sun' -> 7
    end.

dayname(Day) ->
    case Day of
        1 -> 'Mon';
        2 -> 'Tue';
        3 -> 'Wed';
        4 -> 'Thu';
        5 -> 'Fri';
        6 -> 'Sat';
        7 -> 'Sun'
    end.



month_name({{_, Mon, _}, _}) ->
    case Mon of
        1 -> 'Jan';
        2 -> 'Feb';
        3 -> 'Mar';
        4 -> 'Apr';
        5 -> 'May';
        6 -> 'Jun';
        7 -> 'Jul';
        8 -> 'Aug';
        9 -> 'Sep';
        10 -> 'Oct';
        11 -> 'Nov';
        12 -> 'Dec'
    end.


-spec(format(string(), calendar:datetime()) -> string()).
format(Pattern, Datetime = {{_, _, _}, {_, _, _}}) -> format(Pattern, Datetime, "");
format(_Pattern, undefined) -> undefined.

format(_Pattern, undefined, binary) -> undefined;
format(Pattern, Datetime, binary) -> xl_convert:to(binary, format(Pattern, Datetime));
format([], _, Acc) -> Acc;
format([$E, $E, $E | Pattern], Dt, Acc) -> format(Pattern, Dt, Acc ++ atom_to_list(day_of_week(Dt)));
format([$d, $d | Pattern], Dt = {{_, _, Date}, _}, Acc) -> format(Pattern, Dt, Acc ++ xl_string:format_number(2, Date));
format([$M, $M, $M | Pattern], Dt, Acc) -> format(Pattern, Dt, Acc ++ atom_to_list(month_name(Dt)));
format([$M, $M | Pattern], Dt = {{_, Mon, _}, _}, Acc) -> format(Pattern, Dt, Acc ++ xl_string:format_number(2, Mon));
format([$y, $y, $y, $y | Pattern], Dt = {{Year, _, _}, _}, Acc) -> format(Pattern, Dt, Acc ++ integer_to_list(Year));
format([$H, $H | Pattern], Dt = {_, {Hour, _, _}}, Acc) -> format(Pattern, Dt, Acc ++ xl_string:format_number(2, Hour));
format([$m, $m | Pattern], Dt = {_, {_, Min, _}}, Acc) -> format(Pattern, Dt, Acc ++ xl_string:format_number(2, Min));
format([$s, $s | Pattern], Dt = {_, {_, _, Sec}}, Acc) -> format(Pattern, Dt, Acc ++ xl_string:format_number(2, Sec));
format([H | Pattern], Dt, Acc) -> format(Pattern, Dt, Acc ++ [H]).

-spec(now_millis() -> pos_integer()).
now_millis() -> now_micros() div 1000.

-spec(now_micros() -> pos_integer()).
now_micros() ->
    {Mega, Secs, Micros} = erlang:now(),
    (Mega * 1000000 + Secs) * 1000000 + Micros.

-define(BASE_DATE, 62167219200).

-spec(ms_to_datetime(integer()) -> calendar:datetime()).
ms_to_datetime(Milliseconds) -> calendar:gregorian_seconds_to_datetime(?BASE_DATE + (Milliseconds div 1000)).

-spec(datetime_to_ms(calendar:datetime()) -> integer()).
datetime_to_ms(DateTime) -> (calendar:datetime_to_gregorian_seconds(DateTime) - ?BASE_DATE) * 1000.

-spec(weekdays() -> [weekday()]).
weekdays() -> ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'].

-spec(whole_day() -> [integer()]).
whole_day() -> lists:seq(0, 23).

-spec(weekdays_order() -> fun((weekday(), weekday()) -> boolean())).
weekdays_order() ->
    Week = weekdays(),
    fun(A, B) ->
        case {xl_lists:index(A, Week), xl_lists:index(B, Week)} of
            {{ok, X}, {ok, Y}} -> X =< Y
        end
    end.

adjust(_Start, Finish, [], _Hours) -> {Finish, Finish};
adjust(_Start, Finish, _Weekdays, []) -> {Finish, Finish};
adjust(Start, Finish, Weekdays, Hours) ->
    case {adjust(Start, datetime_to_ms(Finish), Weekdays, Hours, 1), adjust(Finish, datetime_to_ms(Start), Weekdays, Hours, -1)} of
        {undefined, _} -> {Finish, Finish};
        {_, undefined} -> {Finish, Finish};
        {{ok, S}, {ok, F}} -> {S, F}
    end.

adjust(Date, Limit, Weekdays, Hours, Shift) ->
    case (datetime_to_ms(Date) > Limit andalso Shift == 1) orelse (datetime_to_ms(Date) < Limit andalso Shift == -1) of
        true -> undefined;
        _ ->
            case lists:member(day_of_week(Date), Weekdays) of
                true -> {ok, Date};
                _ -> adjust(add(Date, Shift, days), Limit, Weekdays, Hours, Shift)
            end
    end.


diff_hours(Start, Finish, Weekdays, Hours) when is_integer(Start), is_integer(Finish) ->
    diff_hours(ms_to_datetime(Start), ms_to_datetime(Finish), Weekdays, Hours);
diff_hours(Start = {_, {SH, _, _}}, Finish = {_, {FH, _, _}}, Weekdays, Hours) ->
    case diff_days(Start, Finish, Weekdays) of
        1 -> xl_lists:count(fun(H) -> lists:member(H, Hours) end, lists:seq(SH, FH));
        Days ->
            FirstDayHoursMinus = case lists:member(day_of_week(Start), Weekdays) of
                true -> length(Hours) - xl_lists:count(fun(H) -> lists:member(H, Hours) end, lists:seq(SH, 23));
                _ -> 0
            end,
            LastDayHoursMinus = case lists:member(day_of_week(Finish), Weekdays) of
                true -> length(Hours) - xl_lists:count(fun(H) -> lists:member(H, Hours) end, lists:seq(0, FH));
                _ -> 0
            end,
            Days * length(Hours) - FirstDayHoursMinus - LastDayHoursMinus
    end.

diff_days(Start, Finish, Weekdays) when is_integer(Start), is_integer(Finish) ->
    diff_days(ms_to_datetime(Start), ms_to_datetime(Finish), Weekdays);
diff_days(Start = {StartDate, _}, Finish = {FinishDate, _}, Weekdays) ->
    FirstDay = daynum_of_week(Start),
    LastDay = daynum_of_week(Finish),
    TotalDays = calendar:date_to_gregorian_days(FinishDate) - calendar:date_to_gregorian_days(StartDate),
    case TotalDays div 7 of
        0 when LastDay >= FirstDay ->
            xl_lists:count(fun(D) -> lists:member(dayname(D), Weekdays) end, lists:seq(FirstDay, LastDay));
        Weeks ->
            xl_lists:count(fun(D) -> lists:member(dayname(D), Weekdays) end, lists:seq(FirstDay, 7))
                + Weeks * length(Weekdays)
                + xl_lists:count(fun(D) -> lists:member(dayname(D), Weekdays) end, lists:seq(1, LastDay))
    end.

