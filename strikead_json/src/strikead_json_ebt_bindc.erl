-module(strikead_json_ebt_bindc).

-compile({parse_transform, do}).

-behaviour(ebt_task).

-export([perform/2]).

perform(Dir, _Config) ->
    do([error_m ||
        Bindings <- return(filelib:wildcard(Dir ++ "/src/*.bind")),
        strikead_lists:eforeach(fun(File) ->
            strikead_json_bindc:compile(File, Dir)
        end, Bindings)
    ]).
