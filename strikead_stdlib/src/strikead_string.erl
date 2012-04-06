-module(strikead_string).
-export([empty/1, not_empty/1, strip/1, quote/1, stripthru/1, format/2,
	to_float/1, substitute/2, to_string/1]).

-spec empty/1 :: (string()) -> boolean().
empty(S) -> S == "".

-spec not_empty/1 :: (string()) -> boolean().
not_empty(S) -> S /= "".

-spec strip/1 :: (string()) -> string().
strip(S) -> strip(S, forward).
strip("", _) -> "";
strip([$ | T], Dir) -> strip(T, Dir);
strip([$\t | T], Dir) -> strip(T, Dir);
strip([$\r | T], Dir) -> strip(T, Dir);
strip([$\n | T], Dir) -> strip(T, Dir);
strip(T, forward) -> lists:reverse(strip(lists:reverse(T), backward));
strip(T, backward) -> T.

-spec stripthru/1 :: (string()) -> string().
stripthru(S) -> [X || X <- S, X /= $\n andalso X /= $\r andalso X /= $\t].

-spec quote/1 :: (string()) -> string().
quote(Str) -> format("~5000p", [Str]).

-spec format/2 :: (io:format(), [term()]) -> string().
format(Pattern, Values) -> lists:flatten(io_lib:format(Pattern, Values)).

-spec to_float/1 :: (string()) -> float().
to_float(X) ->
	try
		list_to_float(X)
	catch
		_:_ -> float(list_to_integer(X))
	end.

-type subst_map() :: [{atom(), atom() | binary() | string() |
	integer() | float() | boolean()}].
-spec substitute/2 :: (string(), subst_map()) -> string().
substitute(Str, Map) ->
	Parts = re:split(Str, "(\{[a-zA-Z\-_]+\})", [{return, list}, trim]),
	lists:flatten([replace_macro(X, Map) || X <- Parts]).

-spec replace_macro/2 :: (string(), subst_map()) -> string().
replace_macro([${|T], Map) ->
	Key = list_to_atom(string:strip(T, right, $})),
	case lists:keyfind(Key, 1, Map) of
		{_, V} -> to_string(V);
		_ -> ""
	end;
replace_macro(X, _Map) -> X.

-spec to_string/1 :: (atom() | binary() | string() | float() | integer())
	-> string().
to_string(V) when is_binary(V) -> binary_to_list(V);
to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(V) when is_list(V) -> V;
to_string(V) when is_float(V); is_integer(V) -> format("~p", [V]);
to_string(V) -> format("~p", [V]).

