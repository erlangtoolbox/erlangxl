-module(xl_json).

-export([to_json/1, from_json/1]).
%%binder rt
-export([ktuo_find/3, ktuo_find/4]).

-spec to_json/1 :: (Param) -> string()
    when Param :: (binary() | atom() | number() | [Param] | [{Param, Param}]).
to_json(<<>>) -> "\"\"";
to_json(X) when is_binary(X) -> xl_string:format("~p", [binary_to_list(X)]);
to_json(X) when is_number(X) -> xl_string:format("~p", [X]);
to_json(true) -> "true";
to_json(false) -> "false";
to_json(undefined) -> "null";
to_json(X) when is_atom(X) -> xl_string:format("\"~s\"", [X]);
to_json(X = [H | _]) when is_tuple(H) andalso size(H) == 2 ->
    "{" ++ string:join([xl_string:format("\"~s\":", [K]) ++ to_json(V)
        || {K, V} <- X], ",") ++ "}";
to_json(X) when is_list(X) -> "[" ++ string:join([to_json(E) || E <- X], ",") ++ "]".

-spec from_json/1 :: (string()) -> error_m:monad(any()).
from_json(Value) ->
    case ktj_parse:parse(Value) of
        {Json, _, _} -> {ok, ktuo_transform(Json)};
        E = {error, _} -> E
    end.

-spec ktuo_transform/1 :: (any()) -> any().
ktuo_transform({obj, X}) ->
    [{xl_convert:to_atom(K), ktuo_transform(V)} || {K, V} <- X];
ktuo_transform(X) when is_list(X) ->
    [ktuo_transform(V) || V <- X];
ktuo_transform(null) -> undefined;
ktuo_transform(X) -> X.

-type prim_type() :: string | integer | float | atom | boolean.
-type type() :: prim_type() | [prim_type()] | undefined.

-spec ktuo_find/3 :: (atom(), {obj, [{binary(), term()}]}, type()) -> term().
ktuo_find(Field, Obj, Type) -> ktuo_find(Field, Obj, undefined, Type).

-spec ktuo_find/4 :: (atom(), {obj, [{binary(), term()}]}, term(), type()) -> term().
ktuo_find(_Field, undefined, Default, _Type) -> Default;
ktuo_find(Field, {obj, Fields}, Default, Type) when is_list(Fields) ->
    case xl_lists:kvfind(atom_to_binary(Field, utf8), Fields) of
        {ok, null} -> undefined;
        {ok, V} when Type == atom -> binary_to_atom(V, utf8);
        {ok, V} when Type == {list, atom} -> lists:map(fun(X) -> binary_to_atom(X, utf8) end, V);
        {ok, V} -> V;
        _ -> Default
    end.
