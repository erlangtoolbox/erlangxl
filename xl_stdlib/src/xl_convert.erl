%% Copyright
-module(xl_convert).
-author("volodymyr.kyrychenko@strikead.com").

-type primitive_type() :: atom() | binary() | string() | float() | integer().

%% API
-export([to_string/1, to_atom/1, to_float/1, to_binary/1, to_integer/1, make_atom/1]).

-spec to_string/1 :: (primitive_type()) -> string().
to_string(V) when is_binary(V) -> binary_to_list(V);
to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(V) when is_list(V) -> V;
to_string(V) -> lists:flatten(io_lib:format("~p", [V])).


-spec to_atom/1 :: (binary() | string() | atom()) -> atom().
to_atom(X) when is_binary(X) -> binary_to_atom(X, utf8);
to_atom(X) when is_list(X) -> list_to_atom(X);
to_atom(X) when is_atom(X) -> X.

-spec to_float/1 :: (string() | binary()) -> float().
to_float(X) when is_binary(X) -> to_float(binary_to_list(X));
to_float(X) ->
    try
        list_to_float(X)
    catch
        _:_ -> float(list_to_integer(X))
    end.

-spec to_binary/1 :: (primitive_type()) -> binary().
to_binary(X) when is_float(X) -> to_binary(io_lib:format("~p", X));
to_binary(X) when is_integer(X) -> to_binary(integer_to_list(X));
to_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_binary(X) -> X.

-spec to_integer/1 :: (string() | atom() | binary()) -> integer().
to_integer(X) when is_list(X) -> list_to_integer(X);
to_integer(X) when is_atom(X) -> list_to_integer(atom_to_list(X));
to_integer(X) when is_binary(X) -> list_to_integer(binary_to_list(X)).

-spec make_atom/1 :: ([primitive_type()]) -> atom().
make_atom(L) -> list_to_atom(string:join([to_string(X) || X <- L], "")).
