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

any_test() ->
    Args = mkargs(),
    ?assertEqual({[{a, "b"}],[]}, strikead_yaws:params([strikead_yaws:any([strikead_yaws:get(x), strikead_yaws:opt(a)], "Not found")], Args)).

any_as_test() ->
    Args = mkargs(),
    ?assertEqual({[{z, "b"}],[]}, strikead_yaws:params([strikead_yaws:any_as(z, [strikead_yaws:get(x), strikead_yaws:opt(a)])], Args)).


mkargs() ->
    #arg{
        req = #http_request{method='GET', path="/test", version="HTTP/1.1"},
        querydata="a=b&c=d"
    }.
