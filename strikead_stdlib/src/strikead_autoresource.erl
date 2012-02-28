-module(strikead_autoresource).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{open,1},
     {close,1}];

behaviour_info(_) -> undefined.
