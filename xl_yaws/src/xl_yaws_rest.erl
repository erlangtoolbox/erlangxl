-module(xl_yaws_rest).

-include_lib("yaws/include/yaws_api.hrl").

-export([dispatch/2]).

dispatch(Module, Args) ->
    {Func, Params} = case string:tokens(Args#arg.pathinfo, "/") of
        [Action] -> {list_to_atom(Action), [Args]};
        [Action | Path] -> {list_to_atom(Action), [Path, Args]}
    end,
    case xl_lists:keyfind(Func, 1, Module:module_info(exports)) of
        {ok, {Func, Arity}} when length(Params) == Arity ->
            apply(Module, Func, Params);
        _ -> xl_yaws_errors:out404(Args)
    end.
