-module(persist_storage_bin, [Location]).

-compile({parse_transform, do}).

-behaviour(persist_storage).

-export([load/0, store/2, delete/1]).

-spec load/0 :: () -> error_m:monad([term()]).
load() ->
    do([error_m ||
        xl_file:mkdirs(Location),
        Files <- xl_file:list_dir(Location, "*.bin"),
        xl_lists:emap(fun(F) ->
            do([error_m ||
                Content <- xl_file:read_file(filename:join(Location, F)),
                return(binary_to_term(Content))
            ])
        end, Files)
    ]).

-spec store/2 :: (xl_string:iostring(), term()) -> error_m:monad(ok).
store(Id, X) ->
    xl_file:write_file(
        xl_string:join([Location, "/", Id, ".bin"]),
        term_to_binary(X)).

-spec delete/1 :: (xl_string:iostring()) -> error_m:monad(ok).
delete(Id) ->
    xl_file:delete(xl_string:join([Location, "/", Id, ".bin"])).


