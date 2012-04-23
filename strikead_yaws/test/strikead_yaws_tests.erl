-module(strikead_yaws_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("yaws/include/yaws_api.hrl").

get_test() ->
    Args = mkargs(),
    ?assertEqual({[{a, "b"}],[]}, strikead_yaws:params([strikead_yaws:get(a)], Args)).


opt_test() ->
    Args = mkargs(),
    ?assertEqual({[{b, undefined}],[]}, strikead_yaws:params([strikead_yaws:opt(b)], Args)),
    Xf = fun(X) -> X ++ "-x" end,
    ?assertEqual({[{b, undefined}],[]}, strikead_yaws:params([strikead_yaws:opt(b, Xf)], Args)),
    ?assertEqual({[{a, "b-x"}],[]}, strikead_yaws:params([strikead_yaws:opt(a, Xf)], Args)).

convert_test() ->
    Args = mkargs(),
    Xf = fun(X) -> X ++ "-x" end,
    ?assertEqual({[{a, "b-x"}],[]}, strikead_yaws:params([strikead_yaws:convert(strikead_yaws:opt(a), Xf)], Args)).

any_test() ->
    Args = mkargs(),
    ?assertEqual({[{a, "b"}],[]}, strikead_yaws:params([strikead_yaws:any([strikead_yaws:get(x), strikead_yaws:opt(a)], "Not found")], Args)).

as_test() ->
    Args = mkargs(),
    ?assertEqual({[{z, "b"}],[]}, strikead_yaws:params([strikead_yaws:as(z, strikead_yaws:any([strikead_yaws:get(x), strikead_yaws:opt(a)], "Not Found"))], Args)).


parse_params_test() ->
    ?assertEqual([{a,"b"},{c,"d"}], strikead_yaws:parse_params(mkargs())).

mkargs(Query) ->
    #arg{
        req = #http_request{method='GET', path="/test", version="HTTP/1.1"},
        querydata = string:join(lists:map(fun({K,V}) -> atom_to_list(K) ++ "=" ++ V end, Query), "&")
    }.

mkargs() -> mkargs([{a,"b"},{c,"d"}]).
