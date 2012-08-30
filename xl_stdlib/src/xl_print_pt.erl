-module(xl_print_pt).
-export([parse_transform/2]).
parse_transform(Forms, _Options) ->
    io:format("~p~n", [Forms]),
    Forms.



