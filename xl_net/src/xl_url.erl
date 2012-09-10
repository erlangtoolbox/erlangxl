-module(xl_url).

-export([to_query/1, escape_params/1, escape/1, substitute/2, domain/2]).

-spec to_query/1 :: ([{atom(), term()}]) -> string().
to_query(List) ->
    string:join(
        [atom_to_list(Key) ++ "=" ++ escape(Value) || {Key, Value} <- List],
        "&").

-spec escape_params/1 :: (xl_lists:kvlist_at())
        -> xl_lists:kvlist_at().
escape_params(List) ->
    lists:map(fun({K, V}) -> {K, escape(V)} end, List).

-spec escape/1 :: (term()) -> string().
escape(V) -> edoc_lib:escape_uri(xl_convert:to_string(V)).

-spec substitute/2 :: (string(), xl_lists:kvlist_at()) -> string().
substitute(Url, Map) ->
    xl_string:substitute(Url, escape_params(Map)).

-spec domain(pos_integer(), string()) -> string().
domain(Level, Domain) ->
    string:join(lists:reverse(element(1, xl_lists:split(Level,
        lists:reverse(string:tokens(Domain, "."))))), ".").

