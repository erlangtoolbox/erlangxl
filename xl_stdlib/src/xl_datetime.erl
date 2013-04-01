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

%% @doc Format and Parse dates in erlang
%%
%% Licensed under the DWTFYW License
%%
%% This module formats erlang dates in the form
%% {{Year, Month, Day}, {Hour, Minute, Second}}
%% to printable strings, using (almost) equivalent
%% formatting rules as http://uk.php.net/date
%%
%% It also has an unspecified date parser that
%% will be added to
%%
%% See tests at bottom for examples

%% Modified by andrei.zavada@strikead.com as follows:
%% 1. Removed format functions (use those in xl_calendar:);
%% 2. Extended parse to handle milliseconds.
%% 3. Fixed is_meridian, is_sep macros.

-module(xl_datetime).
-author("Dale Harvey <dale@hypernumbers.com>").

-export([list_to_unixtime/1, parse/1, parse/2]).

-define( is_num(X),      (X >= $0 andalso X =< $9)).
-define( is_meridian(X), (X==[] orelse X==[am] orelse X==[pm]) ).
-define( is_sep(X),      (X==$- orelse X==$/) ).

-type year()     :: non_neg_integer().
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 0..23.
-type minute()   :: 0..59.
-type second()   :: float().
-type date()     :: {year(),month(),day()}.
-type time()     :: {hour(),minute(),second()}.
-type datetime() :: {date(),time()}.
-type now()      :: {integer(),integer(),integer()}.
%%
%% EXPORTS
%%

-spec list_to_unixtime(string()) -> float().
list_to_unixtime(X) ->
    case parse(X) of
        {error, bad_date} ->
            throw(bad_format);
        {{Ye, Mo, Da}, {Ho, Mi, Se}, Millis} ->
            Elapsed =
                calendar:datetime_to_gregorian_seconds({{Ye, Mo, Da}, {Ho, Mi, Se}}) -
                calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
            Elapsed + Millis
    end.


-spec parse(string()) -> datetime().
%% @doc parses the datetime from a string
parse(Date) ->
    do_parse(Date, calendar:universal_time(), []).

-spec parse(string(),datetime() | now()) -> datetime().
%% @doc parses the datetime from a string
parse(Date, {_,_,_}=Now) ->
    do_parse(Date, calendar:now_to_datetime(Now), []);
parse(Date, Now) ->
    do_parse(Date, Now, []).

do_parse(Date, Now, Opts) ->
    case parse(tokenise(string:to_upper(Date), []), Now, Opts) of
        {error, bad_date} ->
            {error, bad_date};
        {D1, T1, Millis} = {{_, _, _}, {_, _, _}, _} ->
            case calendar:valid_date(D1) of
                true  -> {D1, T1, Millis};
                false -> {error, bad_date}
            end;
        _ -> {error, bad_date}
    end.

%%
%% LOCAL FUNCTIONS
%%

%% %% Times - 21:45, 13:45:54, 13:15PM etc
%% parse([Hour,$:,Min,$:,Sec,$.,Millis | PAM], {Date, _Time}, _O) when is_float(Millis), ?is_meridian(PAM) ->
%%     {Date, {hour(Hour, PAM), Min, Sec + Millis}};
%% parse([Hour,$:,Min,$:,Sec | PAM], {Date, _Time}, _O) when ?is_meridian(PAM) ->
%%     {Date, {hour(Hour, PAM), Min, Sec}};
%% parse([Hour,$:,Min | PAM], {Date, {_H,_M,S}}, _Opts) when ?is_meridian(PAM) ->
%%     {Date, {hour(Hour, PAM), Min, S}};

%% Dates 23/april/1963
parse([Day,Month,Year], {_Date, Time}, _Opts)  ->
    {{Year, Month, Day}, Time, 0.0};
parse([Day,X,Month,X,Year], {_Date, Time}, _Opts) when ?is_sep(X) ->
    {{Year, Month, Day}, Time, 0.0};

%% Date/Times 22 Aug 2008 6:35 PM
parse([Day,X,Month,X,Year, Hour,$:,Min,$:,Sec,Millis | PAM], _Now, _Opts)
  when ?is_meridian(PAM), ?is_sep(X), is_float(Millis), Year > 999, Day =< 31 ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}, Millis};
parse([Day,X,Month,X,Year, Hour,$:,Min,$:,Sec | PAM], _Now, _Opts)
  when ?is_meridian(PAM), ?is_sep(X), Year > 999, Day =< 31 ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}, 0.0};
parse([Day,X,Month,X,Year, Hour,$:,Min | PAM], {_Date, {_H,_M, Sec}}, _Opts)
  when ?is_meridian(PAM), ?is_sep(X), Year > 999, Day =< 31 ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}, 0.0};

parse([Year,X,Month,X,Day, Hour,$:,Min,$:,Sec,Millis | PAM], _Now, _Opts)
  when is_float(Millis), ?is_meridian(PAM), ?is_sep(X) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}, Millis};
parse([Year,X,Month,X,Day, Hour,$:,Min,$:,Sec | PAM], _Now, _Opts)
  when ?is_meridian(PAM), ?is_sep(X) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}, 0.0};
parse([Year,X,Month,X,Day, Hour,$:,Min | PAM], {_Date, {_H,_M, Sec}}, _Opts)
  when ?is_meridian(PAM), ?is_sep(X) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}, 0.0};

%% parse([Day,Month,Year,Hour,$:,Min | PAM], {_Date, {_H,_M, Sec}}, _Opts)
%%   when ?is_meridian(PAM) ->
%%     {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}};
%% parse([Day,Month,Year,Hour,$:,Min,$:,Sec,Millis | PAM], _Now, _Opts)
%%   when is_float(Millis), ?is_meridian(PAM) ->
%%     {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec+Millis}};
%% parse([Day,Month,Year,Hour,$:,Min,$:,Sec | PAM], _Now, _Opts)
%%   when ?is_meridian(PAM) ->
%%     {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}};

%% 2010-09-01T20:49:05
parse([Year, $-, Month, $-, Day, {bad_token,84}, Hour, $:, Min, $:, Sec, Millis], _Now, _Opts) ->
    {{Year, Month, Day}, {Hour, Min, Sec}, Millis};
%% 2010-09-01T20:49:05.185Z
parse([Year, $-, Month, $-, Day, {bad_token,84}, Hour, $:, Min, $:, Sec | _Rest], _Now, _Opts) ->
    {{Year, Month, Day}, {Hour, Min, Sec}};

parse(_Tokens, _Now, _Opts) ->
    {error, bad_date}.

tokenise([], Acc) ->
    lists:reverse(Acc);

tokenise([N1, N2, N3, N4 | Rest], Acc)
  when ?is_num(N1), ?is_num(N2), ?is_num(N3), ?is_num(N4) ->
    tokenise(Rest, [ ltoi([N1, N2, N3, N4]) | Acc]);
tokenise([N1, N2 | Rest], Acc)
  when ?is_num(N1), ?is_num(N2) ->
    tokenise(Rest, [ ltoi([N1, N2]) | Acc]);
tokenise([N1 | Rest], Acc)
  when ?is_num(N1)  ->
    tokenise(Rest, [ ltoi([N1]) | Acc]);

tokenise([$., N1, N2, N3 | Rest], Acc)
  when ?is_num(N1), ?is_num(N2), ?is_num(N3) ->
    tokenise(Rest, [ list_to_float([$0, $., N1, N2, N3]) | Acc]);
tokenise([$., N1, N2 | Rest], Acc)
  when ?is_num(N1), ?is_num(N2) ->
    tokenise(Rest, [ list_to_float([$0, $., N1, N2]) | Acc]);
tokenise([$., N1 | Rest], Acc)
  when ?is_num(N1) ->
    tokenise(Rest, [ list_to_float([$0, $., N1]) | Acc]);

tokenise("JANUARY"++Rest, Acc)   -> tokenise(Rest, [1 | Acc]);
tokenise("JAN"++Rest, Acc)       -> tokenise(Rest, [1 | Acc]);
tokenise("FEBUARY"++Rest, Acc)   -> tokenise(Rest, [2 | Acc]);
tokenise("FEB"++Rest, Acc)       -> tokenise(Rest, [2 | Acc]);
tokenise("MARCH"++Rest, Acc)     -> tokenise(Rest, [3 | Acc]);
tokenise("MAR"++Rest, Acc)       -> tokenise(Rest, [3 | Acc]);
tokenise("APRIL"++Rest, Acc)     -> tokenise(Rest, [4 | Acc]);
tokenise("APR"++Rest, Acc)       -> tokenise(Rest, [4 | Acc]);
tokenise("MAY"++Rest, Acc)       -> tokenise(Rest, [5 | Acc]);
tokenise("JUNE"++Rest, Acc)      -> tokenise(Rest, [6 | Acc]);
tokenise("JUN"++Rest, Acc)       -> tokenise(Rest, [6 | Acc]);
tokenise("JULY"++Rest, Acc)      -> tokenise(Rest, [7 | Acc]);
tokenise("JUL"++Rest, Acc)       -> tokenise(Rest, [7 | Acc]);
tokenise("AUGUST"++Rest, Acc)    -> tokenise(Rest, [8 | Acc]);
tokenise("AUG"++Rest, Acc)       -> tokenise(Rest, [8 | Acc]);
tokenise("SEPTEMBER"++Rest, Acc) -> tokenise(Rest, [9 | Acc]);
tokenise("SEP"++Rest, Acc)       -> tokenise(Rest, [9 | Acc]);
tokenise("OCTOBER"++Rest, Acc)   -> tokenise(Rest, [10 | Acc]);
tokenise("OCT"++Rest, Acc)       -> tokenise(Rest, [10 | Acc]);
tokenise("NOVEMBER"++Rest, Acc)  -> tokenise(Rest, [11 | Acc]);
tokenise("NOV"++Rest, Acc)       -> tokenise(Rest, [11 | Acc]);
tokenise("DECEMBER"++Rest, Acc)  -> tokenise(Rest, [12 | Acc]);
tokenise("DEC"++Rest, Acc)       -> tokenise(Rest, [12 | Acc]);


tokenise("MONDAY"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("MON"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("TUESDAY"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("TUE"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("WEDNESDAY"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("WED"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("THURSDAY"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("THU"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("FRIDAY"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("FRI"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("SATURDAY"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("SAT"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("SUNDAY"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("SUN"++Rest, Acc)       -> tokenise(Rest, Acc);


tokenise([$: | Rest], Acc) -> tokenise(Rest, [ $: | Acc]);
tokenise([$. | Rest], Acc) -> tokenise(Rest, [ $. | Acc]);
tokenise([$/ | Rest], Acc) -> tokenise(Rest, [ $/ | Acc]);
tokenise([$- | Rest], Acc) -> tokenise(Rest, [ $- | Acc]);
tokenise("AM"++Rest, Acc)  -> tokenise(Rest, [am | Acc]);
tokenise("PM"++Rest, Acc)  -> tokenise(Rest, [pm | Acc]);

tokenise([$  | Rest], Acc) -> tokenise(Rest, Acc);          % Spaces
tokenise("TH"++Rest, Acc)  -> tokenise(Rest, Acc);
tokenise("ND"++Rest, Acc)  -> tokenise(Rest, Acc);
tokenise("ST"++Rest, Acc)  -> tokenise(Rest, Acc);
tokenise("OF"++Rest, Acc)  -> tokenise(Rest, Acc);

tokenise([Else | Rest], Acc) ->
    tokenise(Rest, [{bad_token, Else} | Acc]).

hour(Hour, []) -> Hour;
hour(Hour, [am]) -> Hour;
hour(Hour, [pm]) -> Hour+12.


%%% uncomment on demand
%% -spec iso_week(date()) -> integer().
%% %% @doc The week of the years as defined in ISO 8601
%% %%      http://en.wikipedia.org/wiki/ISO_week_date
%% iso_week(Date) ->
%%     Week = iso_week_one(iso_year(Date)),
%%     Days = calendar:date_to_gregorian_days(Date) -
%%         calendar:date_to_gregorian_days(Week),
%%     trunc((Days / 7) + 1).

%% -spec iso_year(date()) -> integer().
%% %% @doc The year number as defined in ISO 8601
%% %%      http://en.wikipedia.org/wiki/ISO_week_date
%% iso_year({Y, _M, _D}=Dt) ->
%%     case Dt >= {Y, 12, 29} of
%%         true ->
%%             case Dt < iso_week_one(Y+1) of
%%                 true  -> Y;
%%                 false -> Y+1
%%             end;
%%         false ->
%%             case Dt < iso_week_one(Y) of
%%                 true  -> Y-1;
%%                 false -> Y
%%             end
%%     end.

%% -spec iso_week_one(year()) -> date().
%% %% @doc The date of the the first day of the first week
%% %%      in the ISO calendar
%% iso_week_one(Y) ->
%%     Day1 = calendar:day_of_the_week({Y,1,4}),
%%     Days = calendar:date_to_gregorian_days({Y,1,4}) + (1-Day1),
%%     calendar:gregorian_days_to_date(Days).

ltoi(X) ->
    list_to_integer(X).

%%
%% TEST FUNCTIONS
%%
%% c(dh_date,[{d,'TEST'}]).
%-define(NOTEST, 1).

-include_lib("eunit/include/eunit.hrl").

-define(DATE, {{2001,3,10},{17,16,17}}).
-define(ISO,  "o \\WW").

basic_parse_test_() -> [
  ?_assertEqual({{2008,8,22}, {17,16,17}},
                parse("22nd of August 2008", ?DATE)),
  ?_assertEqual({{2008,8,22}, {6,35,17}},
                parse("22-Aug-2008 6:35 AM", ?DATE)),
  ?_assertEqual({{2008,8,22}, {6,35,12}},
                parse("22-Aug-2008 6:35:12 AM", ?DATE)),
  ?_assertEqual({{2008,8,22}, {6,35,17}},
                parse("22/Aug/2008 6:35 AM", ?DATE)),
  ?_assertEqual({{2008,8,22}, {6,35,17}},
                parse("22/August/2008 6:35 AM", ?DATE)),
  ?_assertEqual({{2008,8,22}, {6,35,17}},
                parse("22 August 2008 6:35 AM", ?DATE)),
  ?_assertEqual({{2008,8,22}, {6,35,17}},
                parse("22 Aug 2008 6:35AM", ?DATE)),
  ?_assertEqual({{2008,8,22}, {6,35,17}},
                parse("22 Aug 2008 6:35 AM", ?DATE)),
  ?_assertEqual({{2008,8,22}, {6,35,17}},
                parse("22 Aug 2008 6:35", ?DATE)),
  ?_assertEqual({{2008,8,22}, {18,35,17}},
                parse("22 Aug 2008 6:35 PM", ?DATE)),
  ?_assertEqual({{2001,3,10}, {11,15,17}},
                parse("11:15", ?DATE)),
  ?_assertEqual({{2001,3,10}, {1,15,17}},
                parse("1:15", ?DATE)),
  ?_assertEqual({{2001,3,10}, {1,15,17}},
                parse("1:15 am", ?DATE)),
  ?_assertEqual({{2001,3,10}, {3,45,39}},
                parse("3:45:39", ?DATE)),
  ?_assertEqual({{1963,4,23}, {17,16,17}},
                parse("23/4/1963", ?DATE)),
  ?_assertEqual({{1999,4,23}, {17,16,17.016}},
                parse("1999/04/23 17:16:17.016", ?DATE)),
  ?_assertEqual({{1963,4,23}, {17,16,17}},
                parse("23/april/1963", ?DATE)),
  ?_assertEqual({{1963,4,23}, {17,16,17}},
                parse("23/apr/1963", ?DATE)),
  ?_assertEqual({error, bad_date},
                parse("23/ap/195", ?DATE)),
  ?_assertEqual({{2001,3,10}, {6,45,17}},
                parse("6:45 am", ?DATE)),
  ?_assertEqual({{2001,3,10}, {18,45,17}},
                parse("6:45 PM", ?DATE)),
  ?_assertEqual({{2001,3,10}, {18,45,17}},
                parse("6:45 PM ", ?DATE))
].

% Local Variables:
% indent-tabs-mode: nil
% End:
