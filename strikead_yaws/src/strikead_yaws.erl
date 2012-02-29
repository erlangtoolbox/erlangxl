-module(strikead_yaws).

-export([get/1, get/2, any/2, any_as/2, opt/1, opt/2, opt/3, params/2, errors/1]).

get(Name) -> get(Name, io_lib:format("Parameter '~p' must be present", [Name])).

get(Name, Message) ->
    fun(Args) ->
        case yaws_api:getvar(Args, Name) of
            undefined -> {error, Message};
            {ok, X} -> {ok, Name, X};
            X when is_tuple(X) -> {error, io_lib:format("Single parameter '~p' expected", [Name])}
        end
    end.

opt(Name) -> opt(Name, undefined).

opt(Name, Transform) when is_function(Transform)-> opt(Name, undefined, Transform);
opt(Name, Default) -> opt(Name, Default, fun(X) -> X end).

opt(Name, Default, Transform) ->
    fun(Args) ->
        case yaws_api:getvar(Args, Name) of
            undefined -> {ok, Name, Default};
            {ok, X} -> {ok, Name, Transform(X)};
            X when is_tuple(X) -> {error, io_lib:format("Single parameter '~p' expected", [Name])}
        end
    end.

any(Alts, Message) ->
    fun(Args) ->
        case find_any(Alts, Args) of
            not_found -> {error, Message};
            X -> X
        end
    end.

find_any(Alts, Args) ->
    strikead_stream:mapfind(
        fun(F) ->
            case F(Args) of
                {error, _} -> false;
                X -> X
            end
        end, strikead_stream:to_stream(Alts)).

any_as(Name, Alts) ->
    fun(Args) ->
        case find_any(Alts, Args) of
            not_found -> {Name, undefined};
            {ok, _, X} -> {ok, Name, X}
        end
    end.

getparams(_Args, []) -> [];
getparams(Args, [F | T]) -> [F(Args) | getparams(Args, T)].

params(Fs, Args) ->
    {Params, Errors} = lists:partition(fun({_,_,_}) -> true;({error,_})-> false end, getparams(Args, Fs)),
    {lists:map(fun({_, N, V}) -> {N,V} end, Params), lists:map(fun({_, E}) -> E end, Errors)}.

errors(Errors) -> [{status, 400}, {ehtml, {html, [], lists:flatten(lists:map(fun(X) -> [X, {br}] end, Errors))}}].
