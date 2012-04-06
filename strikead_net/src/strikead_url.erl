-module(strikead_url).

-export([to_query/1]).

-spec to_query/1 :: ([{atom(), term()}]) -> string().
to_query(List) ->
	string:join([atom_to_list(Key) ++ "=" ++
	edoc_lib:escape_uri(strikead_string:to_string(Value))
	|| {Key, Value} <- List ], "&").
