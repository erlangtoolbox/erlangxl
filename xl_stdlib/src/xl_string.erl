-module(xl_string).

-export([empty/1, not_empty/1, strip/1, quote/1, unquote/1, stripthru/1, format/2,
    to_float/1, substitute/2, substitute/3, to_string/1, mk_atom/1, to_upper/1,
    to_lower/1, equal_ignore_case/2, join/2, join/1, to_atom/1, to_binary/1,
    to_integer/1, generate_uuid/0, replace/3]).

-type iostring() :: string() | binary().
-export_type([iostring/0]).

-deprecated({to_float, 1}).
-deprecated({to_string, 1}).
-deprecated({mk_atom, 1}).
-deprecated({to_atom, 1}).
-deprecated({to_binary, 1}).
-deprecated({to_integer, 1}).

-spec empty/1 :: (string()) -> boolean().
empty(S) -> S == "".

-spec not_empty/1 :: (string()) -> boolean().
not_empty(S) -> S /= "".

-spec strip/1 :: (string()) -> string().
strip(S) -> lists:reverse(strip_(lists:reverse(strip_(S)))).
strip_("") -> "";
strip_([$  | T]) -> strip(T);
strip_([$\t | T]) -> strip(T);
strip_([$\r | T]) -> strip(T);
strip_([$\n | T]) -> strip(T);
strip_(S) -> S.

-spec stripthru/1 :: (string()) -> string().
stripthru(S) -> [X || X <- S, X /= $\n andalso X /= $\r andalso X /= $\t].

-spec quote/1 :: (string()) -> string().
quote(S) -> format("~5000p", [S]).

-spec unquote/1 :: (string()) -> string().
unquote(S) -> replace(string:strip(S, both, $"), "\\", "").

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

-spec format/2 :: (io:format(), [term()]) -> string().
format(Pattern, Values) -> lists:flatten(io_lib:format(Pattern, Values)).

-spec to_float/1 :: (iostring()) -> float().
to_float(X) -> xl_convert:to_float(X).

-spec substitute/2 :: (string(), xl_lists:kvlist_at()) -> string().
substitute(Str, Map) -> substitute(Str, Map, {${, $}}).

-spec substitute/3 :: (string(), ebt_xl_lists:kvlist_at(), {char(), char()}) -> string().
substitute(Str, Map, {Open, Close}) ->
    Parts = re:split(Str, format("(\\\~s[a-zA-Z0-9_\\.:-]+\\\~s)", [[Open], [Close]]), [{return, list}, trim]),
    lists:flatten([replace_macro(X, Map, {Open, Close}) || X <- Parts]).

-spec replace_macro/3 :: (string(), ebt_xl_lists:kvlist_at(), {char(), char()}) -> string().
replace_macro([Open | T], Map, {Open, Close}) ->
    Key = list_to_atom(string:strip(T, right, Close)),
    case lists:keyfind(Key, 1, Map) of
        {_, V} -> xl_convert:to(string, V);
        _ -> ""
    end;
replace_macro(X, _Map, _) -> X.


-spec to_string/1 :: (atom() | binary() | string() | float() | integer()) -> string().
to_string(X) -> xl_convert:to_string(X).

-spec mk_atom/1 :: ([atom() | binary() | string() | float() | integer()]) -> atom().
mk_atom(L) -> xl_convert:make_atom(L).

-spec to_atom/1 :: (iostring() | atom()) -> atom().
to_atom(X) when is_binary(X) -> binary_to_atom(X, utf8);
to_atom(X) when is_list(X) -> list_to_atom(X);
to_atom(X) when is_atom(X) -> X.

-spec equal_ignore_case/2 :: (iostring(), iostring()) -> boolean().
equal_ignore_case(A, B) ->
    string:equal(to_lower(xl_convert:to(string, A)), to_lower(xl_convert:to(string, B))).

-spec to_lower/1 :: (iostring()) -> iostring().
to_lower(S) when is_binary(S) -> list_to_binary(to_lower(binary_to_list(S)));
to_lower(S) when is_list(S) -> string:to_lower(S).

-spec to_upper/1 :: (iostring()) -> iostring().
to_upper(S) when is_binary(S) -> list_to_binary(to_upper(binary_to_list(S)));
to_upper(S) when is_list(S) -> string:to_upper(S).

%todo test performance of concatenating lists and binaries
-spec join/2 :: ([iostring()], iostring()) -> iostring().
join(List, Delim) when is_binary(Delim) ->
    list_to_binary(join(List, binary_to_list(Delim)));
join(List, Delim) -> string:join([xl_convert:to(string, X) || X <- List], Delim).

-spec join/1 :: ([iostring()]) -> string().
join(List) -> join(List, "").


-spec to_binary/1 :: (integer() | iostring() | atom()) -> binary().
to_binary(X) -> xl_convert:to_binary(X).

-spec to_integer/1 :: (iostring() | atom() | binary()) -> integer().
to_integer(X) -> xl_conver:to_integer(X).

-spec generate_uuid/0 :: () -> binary().
generate_uuid() ->
    hd(flake_harness:generate(1, 62)).
