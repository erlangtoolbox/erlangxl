-module(strikead_json).

-export([to_json/1]).

to_json(X) when is_binary(X) -> strikead_string:format("\"~s\"", [X]);
to_json(X) when is_number(X) -> strikead_string:format("~p", [X]);
to_json(true) -> "true";
to_json(false) -> "false";
to_json(undefined) -> "null";
to_json(X = [H|_]) when is_tuple(H) andalso size(H) == 2 ->
	"{" ++ string:join([strikead_string:format("\"~s\":", [K]) ++ to_json(V)
		|| {K,V} <- X], ",") ++ "}";
to_json(X) when is_list(X) -> "[" ++ string:join([to_json(E) || E <- X], ",") ++ "]".

