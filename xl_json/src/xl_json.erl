-module(xl_json).

-export([to_json/1, from_json/1]).
%%binder rt
-export([ktuo_find/2, ktuo_find/3]).

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
    [{xl_string:to_atom(K), ktuo_transform(V)} || {K, V} <- X];
ktuo_transform(X) when is_list(X) ->
    [ktuo_transform(V) || V <- X];
ktuo_transform(null) -> undefined;
ktuo_transform(X) -> X.

-spec ktuo_find/2 :: (atom(), {obj, [{binary(), term()}]}) -> term().
ktuo_find(Field, Obj) -> ktuo_find(Field, Obj, undefined).

-spec ktuo_find/3 :: (atom(), {obj, [{binary(), term()}]}, term()) -> term().
ktuo_find(_Field, undefined, Default) -> Default;
ktuo_find(Field, {obj, Fields}, Default) when is_list(Fields) ->
    case xl_lists:kvfind(atom_to_binary(Field, utf8), Fields) of
        {ok, null} -> undefined;
        {ok, V} -> V;
        _ -> Default
    end.
