%% Copyright
-module(xl_json_ktuo).
-author("volodymyr.kyrychenko@strikead.com").

-behaviour(xl_json_api).

%% API
-export([from_json/1, get_value/2, to_abstract/1, to_json/1]).

-spec(from_json(iolist()) -> error_m:monad(xl_json_api:json_document())).
from_json(Source) ->
    case ktj_parse:parse(Source) of
        {Json, _, _} -> {ok, Json};
        E = {error, _} -> E
    end.

-spec(get_value(atom(), xl_json_api:json_document()) -> option_m:monad(any())).
get_value(Field, {obj, Fields}) ->
    xl_lists:kvfind(atom_to_binary(Field, utf8), Fields).


-spec(to_abstract(xl_json_api:json_document()) -> xl_json_api:abstract_json_document()).
to_abstract({obj, Fields}) ->
    [{xl_convert:to(atom, K), to_abstract(V)} || {K, V} <- Fields];
to_abstract(List) when is_list(List) -> [to_abstract(V) || V <- List];
to_abstract(null) -> undefined;
to_abstract(X) -> X.

-spec(to_json(xl_json_api:json_document()) -> binary()).
to_json(Doc) -> xl_convert:to(binary, ktj_encode:encode(Doc)).
