-module(xl_yaws_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("yaws/include/yaws_api.hrl").

get_test() ->
    ?assertEqual({[{a, "b"}], []},
        xl_yaws:params([xl_yaws:get(a)], mkargs())),
    ?assertEqual({[], ["Parameter 'x' must be present"]},
        xl_yaws:params([xl_yaws:get(x)], mkargs())).

opt_test() ->
    Args = mkargs(),
    ?assertEqual({[{b, undefined}], []},
        xl_yaws:params([xl_yaws:opt(b)], Args)),
    Xf = fun(X) -> X ++ "-x" end,
    ?assertEqual({[{b, undefined}], []},
        xl_yaws:params([xl_yaws:opt(b, Xf)], Args)),
    ?assertEqual({[{a, "b-x"}], []},
        xl_yaws:params([xl_yaws:opt(a, Xf)], Args)).

convert_test() ->
    Xf = fun(X) -> X ++ "-x" end,
    ?assertEqual({[{a, "b-x"}], []},
        xl_yaws:params([
            xl_yaws:convert(xl_yaws:opt(a), Xf)], mkargs())).

any_test() ->
    ?assertEqual({[{a, "b"}], []},
        xl_yaws:params([xl_yaws:any([
            xl_yaws:get(x), xl_yaws:opt(a)], "Not found")], mkargs())).

as_test() ->
    ?assertEqual({[{z, "b"}], []},
        xl_yaws:params([xl_yaws:as(z, xl_yaws:any([
            xl_yaws:get(x), xl_yaws:opt(a)], "Not Found"))], mkargs())).

list_test() ->
    ?assertEqual({[{x, ["x", "y"]}], []},
        xl_yaws:params([xl_yaws:list(x)], mkargs([{x, "x"}, {x, "y"}]))),
    ?assertEqual({[{x, ["x"]}], []},
        xl_yaws:params([xl_yaws:list(x)], mkargs([{x, "x"}]))),
    ?assertEqual({[{x, []}], []},
        xl_yaws:params([xl_yaws:list(x)], mkargs())).

parse_params_test() ->
    ?assertEqual([{a, "b"}, {c, "d"}], xl_yaws:parse_params(mkargs())).

params_test() ->
    ?assertEqual({[{a, "b"}, {c, "d"}], []}, xl_yaws:params([
        xl_yaws:get(a),
        xl_yaws:get(c)
    ], mkargs())).

mkargs(Query) ->
    xl_yaws:clear_parse_caches(),
    #arg{
        req = #http_request{method = 'GET', path = "/test", version = "HTTP/1.1"},
        querydata = string:join([atom_to_list(K) ++ "=" ++ V || {K, V} <- Query], "&")
    }.

mkargs() -> mkargs([{a, "b"}, {c, "d"}]).

