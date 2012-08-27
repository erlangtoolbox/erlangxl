-module(xl_json_ebt_bindc).

-compile({parse_transform, do}).

-behaviour(ebt_task).

-export([perform/2]).

perform(Dir, _Config) ->
    do([error_m ||
        Bindings <- return(filelib:wildcard(Dir ++ "/src/*.bind")),
        xl_lists:eforeach(fun(File) ->
            xl_json_bindc:compile(File, Dir)
        end, Bindings)
    ]).
