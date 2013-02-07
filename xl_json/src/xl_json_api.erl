%% Copyright
-module(xl_json_api).
-author("volodymyr.kyrychenko@strikead.com").

-opaque json_document() :: any().
-type abstract_json_document() :: any().

-export_types([json_document/0]).

-callback to_json(json_document()) -> binary().
-callback from_json/1 :: (iolist()) -> error_m:monad(json_document()).
-callback get_value/2 :: (atom(), json_document()) -> option_m:monad(any()).
-callback to_abstract/1 :: (json_document()) -> abstract_json_document().
-callback(bind(fun((atom(), term(), tuple()) -> tuple()), tuple(), xl_json_api:json_document()) -> tuple()).
-callback(field_name_presentation(atom()) -> term()).
