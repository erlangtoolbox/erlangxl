-module(xl_calendar).

-export([format/3, format/2, now_millis/0, now_micros/0, add/3, ms_to_datetime/1,
    day_of_week/1, datetime_to_ms/1, weekdays/0]).

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

day_of_week({Date, _}) ->
    case calendar:day_of_the_week(Date) of
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

weekdays() -> ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'].