-module(strikead_calendar).

-export([advance/2, format/2, now_millis/0]).

-spec advance(From, By) -> calendar:datetime() when
    From :: calendar:datetime(),
    By :: calendar:datetime().
advance({{Year, Mon, Date},{Hour, Min, Sec}}, {{YearA, MonA, DateA}, {HourA, MinA, SecA}}) ->
    {AMA, SR} = advance_item(Sec, SecA, 0, 59),
    {AHA, MR} = advance_item(Min, MinA + AMA, 0, 59),
    {ADA, HR} = advance_item(Hour, HourA + AHA, 0, 23),
    {AYDA, AMoA, DR} = advance_date(Year, Mon, Date, DateA + ADA),
    {AYA, MoR} = advance_item(AMoA-1, MonA, 0, 11),
    YR = AYA + AYDA + YearA,
    {{YR,MoR+1,DR}, {HR,MR,SR}}.


advance_item(Value, Adv, Min, Max) -> {(Value + Adv) div (Max - Min + 1), (Value + Adv) rem (Max - Min + 1)}.
advance_date(Year, Mon, Date, Adv) -> advance_date(Year, Mon, Date + Adv).
advance_date(Year, Mon, Adv) ->
    Ldom = calendar:last_day_of_the_month(Year, Mon),
    if
        Adv =< Ldom -> {Year, Mon, Adv};
        true -> case Mon of
            12 -> advance_date(Year + 1, 1, Adv - Ldom);
            _ ->  advance_date(Year, Mon + 1, Adv - Ldom)
        end
    end.

day_of_week_name({Date, _}) ->
    case calendar:day_of_the_week(Date) of
        1 -> "Mon";
        2 -> "Tue";
        3 -> "Wed";
        4 -> "Thi";
        5 -> "Fri";
        6 -> "Sat";
        7 -> "Sun"
    end.

month_name({{_,Mon,_}, _}) ->
    case Mon of
        1 -> "Jan";
        2 -> "Feb";
        3 -> "Mar";
        4 -> "Apr";
        5 -> "May";
        6 -> "Jun";
        7 -> "Jul";
        8 -> "Aug";
        9 -> "Sep";
        10 -> "Oct";
        11 -> "Nov";
        12 -> "Dec"
    end.

-spec format(Pattern, Datetime) -> string() when
    Pattern :: string(),
    Datetime :: calendar:datetime().
format(Pattern, Datetime) -> format(Pattern, Datetime, "" ).

format([], _, Acc) -> Acc;
format([$E,$E,$E | Pattern], Dt, Acc) -> format(Pattern, Dt, Acc ++ day_of_week_name(Dt));
format([$d,$d | Pattern], Dt={{_, _, Date}, _}, Acc) -> format(Pattern, Dt, Acc ++ lists:flatten(io_lib:format("~2.10.0B",[Date])));
format([$M,$M,$M | Pattern], Dt, Acc) -> format(Pattern, Dt, Acc ++ month_name(Dt));
format([$M,$M | Pattern], Dt={{_, Mon, _}, _}, Acc) -> format(Pattern, Dt, Acc ++ lists:flatten(io_lib:format("~2.10.0B",[Mon])));
format([$y,$y,$y,$y | Pattern], Dt={{Year, _, _},_}, Acc) -> format(Pattern, Dt, Acc ++ integer_to_list(Year));
format([$H,$H | Pattern], Dt={_, {Hour, _, _}}, Acc) -> format(Pattern, Dt, Acc ++ lists:flatten(io_lib:format("~2.10.0B",[Hour])));
format([$m,$m | Pattern], Dt={_, {_, Min, _}}, Acc) -> format(Pattern, Dt, Acc ++ lists:flatten(io_lib:format("~2.10.0B",[Min])));
format([$s,$s | Pattern], Dt={_, {_,_,Sec}}, Acc) -> format(Pattern, Dt, Acc ++ lists:flatten(io_lib:format("~2.10.0B",[Sec])));
format([H | Pattern], Dt, Acc) -> format(Pattern, Dt, Acc ++ [H]).

-spec now_millis() -> integer().
now_millis() ->
    {Mega,Secs,Millis} = erlang:now(),
    (Mega * 1000000 + Secs) * 1000 + Millis div 1000.
