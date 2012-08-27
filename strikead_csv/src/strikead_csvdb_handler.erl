-module(strikead_csvdb_handler).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {extract, 1},
        {find, 2},
        {format, 2}
    ];

behaviour_info(_Other) ->
    undefined.

