-module(strikead_yaws_rest).

-include_lib("yaws/include/yaws_api.hrl").

-export([dispatch/2]).

dispatch(Module, Args) ->
    try
        case string:tokens(Args#arg.pathinfo, "/") of
            [Action] -> apply(Module, list_to_atom(Action), [Args]);
            [Action | Path] -> apply(Module, list_to_atom(Action), [Path, Args])
        end
    catch
        error:undef -> strikead_yaws_errors:out404(Args)
    end.

