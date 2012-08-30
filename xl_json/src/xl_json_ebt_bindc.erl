-module(xl_json_ebt_bindc).

-behaviour(ebt_task).

-export([perform/3]).

perform(_Target, Dir, _Config) ->
    Bindings = filelib:wildcard(Dir ++ "/src/*.bind"),
    xl_lists:eforeach(fun(File) ->
        xl_json_bindc:compile(File, Dir)
    end, Bindings).
