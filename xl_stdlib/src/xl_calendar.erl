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
-module(xl_calendar).

-include("xl_calendar.hrl").

-export([format/3, format/2, now_millis/0, now_micros/0, add/3,
    ms_to_datetime/1, day_of_week/1, datetime_to_ms/1, weekdays/0,
    weekdays_order/0, whole_day/0,
    daynum_of_week/1, daynum/1, dayname/1,
    weekdays_member/2, weekdays_mask/1,
    hourly/2, filter_weekdays/2, filter_hours/2,
    seconds_hourly/1]).

-export_type([weekday/0, hour_of_day/0]).

-type(weekday() :: 'Mon' | 'Tue' | 'Wed' | 'Thu' | 'Fri' | 'Sat' | 'Sun').
-type(hour_of_day() :: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23).
-type(month() :: 'Jan' | 'Feb' | 'Mar' | 'Apr' | 'May' | 'Jun' |
'Jul' | 'Aug' | 'Sep' | 'Oct' | 'Nov' | 'Dec').
-type(units() :: seconds | minutes | hours | days | weeks | months | years).

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

-spec(add(calendar:datetime(), integer(), units()) -> calendar:datetime()).
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
    % Rolling back illegal 31,29
        false -> add({{YYYY, MM, DD - 1}, Time}, 0, months)
    end;
add({{YYYY, MM, DD}, Time}, N, months) when N > 0 andalso MM < 12 ->
    add({{YYYY, MM + 1, DD}, Time}, N - 1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N > 0 andalso MM =:= 12 ->
    add({{YYYY + 1, 1, DD}, Time}, N - 1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N < 0 andalso MM > 1 ->
    add({{YYYY, MM - 1, DD}, Time}, N + 1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N < 0 andalso MM =:= 1 ->
    add({{YYYY - 1, 12, DD}, Time}, N + 1, months);
add(Date, N, years) -> add(Date, 12 * N, months).

% end of Jonas Enlund

-spec(daynum_of_week(pos_integer() | calendar:datetime()) -> calendar:daynum()).
daynum_of_week(Date) when is_integer(Date) -> daynum_of_week(ms_to_datetime(Date));
daynum_of_week({Date, _}) -> calendar:day_of_the_week(Date).

-spec(day_of_week(calendar:datetime()) -> weekday()).
day_of_week(Date) -> dayname(daynum_of_week(Date)).

-spec(daynum(weekday()) -> pos_integer()).
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

-spec(dayname(pos_integer()) -> weekday()).
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

-spec(month_name(pos_integer()) -> month()).
month_name(Mon) ->
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
format(Pattern, Datetime, binary) ->
    xl_convert:to(binary, format(Pattern, Datetime));
format([], _, Acc) -> Acc;
format([$E, $E, $E | Pattern], Dt, Acc) ->
    format(Pattern, Dt, Acc ++ atom_to_list(day_of_week(Dt)));
format([$d, $d | Pattern], Dt = {{_, _, Day}, _}, Acc) ->
    format(Pattern, Dt, Acc ++ xl_string:format_number(2, Day));
format([$M, $M, $M | Pattern], Dt = {{_, Mon, _}, _}, Acc) ->
    format(Pattern, Dt, Acc ++ atom_to_list(month_name(Mon)));
format([$M, $M | Pattern], Dt = {{_, Mon, _}, _}, Acc) ->
    format(Pattern, Dt, Acc ++ xl_string:format_number(2, Mon));
format([$y, $y, $y, $y | Pattern], Dt = {{Year, _, _}, _}, Acc) ->
    format(Pattern, Dt, Acc ++ integer_to_list(Year));
format([$H, $H | Pattern], Dt = {_, {Hour, _, _}}, Acc) ->
    format(Pattern, Dt, Acc ++ xl_string:format_number(2, Hour));
format([$m, $m | Pattern], Dt = {_, {_, Min, _}}, Acc) ->
    format(Pattern, Dt, Acc ++ xl_string:format_number(2, Min));
format([$s, $s | Pattern], Dt = {_, {_, _, Sec}}, Acc) ->
    format(Pattern, Dt, Acc ++ xl_string:format_number(2, Sec));
format([H | Pattern], Dt, Acc) ->
    format(Pattern, Dt, Acc ++ [H]).

-spec(now_millis() -> pos_integer()).
now_millis() -> now_micros() div 1000.

-spec(now_micros() -> pos_integer()).
now_micros() ->
    {Mega, Secs, Micros} = erlang:now(),
    (Mega * 1000000 + Secs) * 1000000 + Micros.

-define(BASE_DATE, 62167219200).

-spec(ms_to_datetime(integer()) -> calendar:datetime()).
ms_to_datetime(Milliseconds) ->
    calendar:gregorian_seconds_to_datetime(?BASE_DATE + (Milliseconds div 1000)).

-spec(datetime_to_ms(calendar:datetime()) -> integer()).
datetime_to_ms(DateTime) ->
    (calendar:datetime_to_gregorian_seconds(DateTime) - ?BASE_DATE) * 1000.

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

-spec(hourly(calendar:datetime(), calendar:datetime()) -> [calendar:datetime()]).
hourly(Start = {_, {_, 0, 0}}, Finish) ->
    hourly(datetime_to_ms(Start), datetime_to_ms(Finish), []);
hourly(Start = {D, {H, _, _}}, Finish) ->
    Next = add({D, {H, 0, 0}}, 1, hours),
    hourly(datetime_to_ms(Next), datetime_to_ms(Finish), [{Start, Next}]).

hourly(StartMs, FinishMs, Acc) ->
    case FinishMs - StartMs of
        Diff when Diff > ?MS_IN_HOUR ->
            NextMs = StartMs + ?MS_IN_HOUR,
            New = {ms_to_datetime(StartMs), ms_to_datetime(NextMs)},
            hourly(NextMs, FinishMs, [New | Acc]);
        0 ->
            lists:reverse(Acc);
        _ ->
            New = {ms_to_datetime(StartMs), ms_to_datetime(FinishMs)},
            hourly(FinishMs, FinishMs, [New | Acc])
    end.

-spec(filter_weekdays([{calendar:datetime(), calendar:datetime()}],
        [weekday()]) -> [[calendar:datetime()]]).
filter_weekdays(Hourly, Weekdays) ->
    Pred = fun({Start, _}) -> not lists:member(day_of_week(Start), Weekdays) end,
    lists:filter(Pred, Hourly).

-spec(filter_hours([{calendar:datetime(), calendar:datetime()}],
        [hour_of_day()]) -> [[calendar:datetime()]]).
filter_hours(Hourly, Hours) ->
    Pred = fun({{_, {H, _, _}}, _}) -> not lists:member(H, Hours) end,
    lists:filter(Pred, Hourly).

-spec(seconds_hourly([{calendar:datetime(), calendar:datetime()}]) -> pos_integer()).
seconds_hourly(Hourly) ->
    Millis = [datetime_to_ms(D2) - datetime_to_ms(D1) || {D1, D2} <- Hourly],
    trunc(lists:sum(Millis) / 1000).

weekdays_mask(D) when is_atom(D) -> 1 bsl (daynum(D) - 1);
weekdays_mask(Days) when is_list(Days) -> lists:foldl(fun(D, Bits) ->
    weekdays_mask(D) bor Bits end, 0, Days).

weekdays_member(D, Days) when is_list(Days) ->
    weekdays_member(D, weekdays_mask(Days));
weekdays_member(D, Mask) when is_integer(Mask) ->
    DMask = weekdays_mask(D), DMask band Mask == DMask.
