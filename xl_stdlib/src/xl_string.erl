-module(xl_string).

-export([empty/1, not_empty/1, strip/1, quote/1, unquote/1, stripthru/1, format/2,
    to_float/1, substitute/2, substitute/3, to_string/1, mk_atom/1, to_upper/1,
    to_lower/1, equal_ignore_case/2, join/2, join/1, to_atom/1, to_binary/1,
    to_integer/1, generate_uuid/0, replace/3, unquote/2, format_number/2]).

-type iostring() :: string() | binary().
-export_type([iostring/0]).

-deprecated({to_float, 1}).
-deprecated({to_string, 1}).
-deprecated({mk_atom, 1}).
-deprecated({to_atom, 1}).
-deprecated({to_binary, 1}).
-deprecated({to_integer, 1}).

-spec(empty(string()) -> boolean()).
empty(S) -> S == "".

-spec(not_empty(string()) -> boolean()).
not_empty(S) -> S /= "".

-spec(strip(string()) -> string()).
strip(S) -> lists:reverse(strip_(lists:reverse(strip_(S)))).
strip_("") -> "";
strip_([$  | T]) -> strip(T);
strip_([$\t | T]) -> strip(T);
strip_([$\r | T]) -> strip(T);
strip_([$\n | T]) -> strip(T);
strip_(S) -> S.

-spec(stripthru(string()) -> string()).
stripthru(S) -> [X || X <- S, X /= $\n andalso X /= $\r andalso X /= $\t].

-spec(quote(string()) -> string()).
quote(S) -> format("~5000p", [S]).

-spec(unquote(string()) -> string()).
unquote(S) -> unquote(S, $").

unquote(S, Symbol) -> replace(string:strip(S, both, Symbol), "\\", "").

replace(S, Search, Replace) -> replace(S, Search, Replace, []).
replace([], _Search, _Replace, Acc) -> lists:flatten(Acc);
replace(S, Search, Replace, Acc) ->
    SearchLength = string:len(Search),
    Pos = string:str(S, Search),
    case Pos of
        0 -> replace([], Search, Replace, [Acc | S]);
        _ ->
            replace(string:substr(S, Pos + SearchLength),
                Search, Replace, [Acc, string:substr(S, 1, Pos - 1) | Replace])
    end.

-spec(format(io:format(), [term()]) -> string()).
format(Pattern, Values) -> lists:flatten(io_lib:format(Pattern, Values)).

-spec(to_float(iostring()) -> float()).
to_float(X) -> xl_convert:to_float(X).

-spec(substitute(string(), xl_lists:kvlist_at()) -> string()).
substitute(Str, Map) -> substitute(Str, Map, {${, $}}).

-spec(substitute(string(), ebt_xl_lists:kvlist_at(), {char(), char()}) -> string()).
substitute(Str, Map, Braces) when is_list(Str) -> binary_to_list(substitute(list_to_binary(Str), Map, Braces));
substitute(Str, Map, {Open, Close}) ->
    Parts = re:split(Str, format("(\\\~s[a-zA-Z0-9_\\.:-]+\\\~s)", [[Open], [Close]]), [{return, binary}, trim]),
    join([replace_macro(X, Map, {Open, Close}) || X <- Parts], <<>>).

-spec(replace_macro(string(), ebt_xl_lists:kvlist_at(), {char(), char()}) -> string()).
replace_macro(<<Open:8, T/binary>>, Map, {Open, _Close}) ->
    Key = binary:part(T, 0, byte_size(T) - 1),
    case xl_lists:kvfind(xl_convert:to(atom, Key), Map) of
        {ok, V} -> V;
        undefined -> ""
    end;
replace_macro(X, _Map, _Braces) -> X.


-spec(to_string(atom() | binary() | string() | float() | integer()) -> string()).
to_string(X) -> xl_convert:to_string(X).

-spec(mk_atom([atom() | binary() | string() | float() | integer()]) -> atom()).
mk_atom(L) -> xl_convert:make_atom(L).

-spec(to_atom(iostring() | atom()) -> atom()).
to_atom(X) when is_binary(X) -> binary_to_atom(X, utf8);
to_atom(X) when is_list(X) -> list_to_atom(X);
to_atom(X) when is_atom(X) -> X.

-spec(equal_ignore_case(iostring(), iostring()) -> boolean()).
equal_ignore_case(A, B) ->
    string:equal(to_lower(xl_convert:to(string, A)), to_lower(xl_convert:to(string, B))).

-spec(to_lower(iostring()) -> iostring()).
to_lower(S) when is_binary(S) -> list_to_binary(to_lower(binary_to_list(S)));
to_lower(S) when is_list(S) -> string:to_lower(S).

-spec(to_upper(iostring()) -> iostring()).
to_upper(S) when is_binary(S) -> list_to_binary(to_upper(binary_to_list(S)));
to_upper(S) when is_list(S) -> string:to_upper(S).

-spec(join([iostring()], iostring()) -> iostring()).
join([], Delim) when is_binary(Delim) -> <<>>;
join([], Delim) when is_list(Delim) -> "";
join([H | T], Delim) when is_binary(Delim) -> join(T, xl_convert:to(binary, H), Delim);
join([H | T], Delim) -> join(T, xl_convert:to(string, H), Delim).

-spec(join([iostring()]) -> string()).
join(List) -> join(List, "").

join([], Acc, Delim) when is_binary(Delim) -> Acc;
join([H | T], Acc, <<>>) ->
    Value = xl_convert:to(binary, H),
    join(T, <<Acc/binary, Value/binary>>, <<>>);
join([H | T], Acc, Delim) when is_binary(Delim) ->
    Value = xl_convert:to(binary, H),
    join(T, <<Acc/binary, Delim/binary, Value/binary>>, Delim);

join([], Acc, Delim) when is_list(Delim) -> Acc;
join([H | T], Acc, "") -> join(T, Acc ++ xl_convert:to(string, H), "");
join([H | T], Acc, Delim) when is_list(Delim) ->
    join(T, Acc ++ Delim ++ xl_convert:to(string, H), Delim).


-spec(to_binary(integer() | iostring() | atom()) -> binary()).
to_binary(X) -> xl_convert:to_binary(X).

-spec(to_integer(iostring() | atom() | binary()) -> integer()).
to_integer(X) -> xl_conver:to_integer(X).

-spec(generate_uuid() -> binary()).
generate_uuid() ->
    hd(flake_harness:generate(1, 62)).

format_number(1, Num) when is_integer(Num) -> integer_to_list(Num);
format_number(2, Num) when is_integer(Num), Num < 10 -> [$0, Num + $0];
format_number(2, Num) when is_integer(Num), Num > 9 -> integer_to_list(Num).
