%% Copyright
-module(xl_json_jiffy).
-author("volodymyr.kyrychenko@strikead.com").

-behaviour(xl_json_api).

%% API
-export([from_json/1, get_value/2, to_abstract/1, to_json/1]).

-spec from_json/1 :: (iolist()) -> error_m:monad(xl_json_api:json_document()).
from_json(Source) ->
    try
        {ok, jiffy:decode(Source)}
    catch
        _:E -> {error, E}
    end.

-spec get_value/2 :: (atom(), xl_json_api:json_document()) -> option_m:monad(any()).
get_value(Field, {Fields}) ->
    xl_lists:kvfind(atom_to_binary(Field, utf8), Fields).


-spec to_abstract/1 :: (xl_json_api:json_document()) -> xl_json_api:abstract_json_document().
to_abstract({Fields}) ->
    [{xl_convert:to(atom, K), to_abstract(V)} || {K, V} <- Fields];
to_abstract(List) when is_list(List) -> [to_abstract(V) || V <- List];
to_abstract(null) -> undefined;
to_abstract(X) -> X.

-spec to_json/1 :: (xl_json_api:json_document()) -> binary().
to_json(Doc) -> jiffy:encode(Doc).
