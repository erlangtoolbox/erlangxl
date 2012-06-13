-module(strikead_yaws_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("yaws/include/yaws_api.hrl").

get_test() ->
    ?assertEqual({[{a, "b"}],[]},
		strikead_yaws:params([strikead_yaws:get(a)], mkargs())),
    ?assertEqual({[],["Parameter 'x' must be present"]},
		strikead_yaws:params([strikead_yaws:get(x)], mkargs())).

opt_test() ->
    Args = mkargs(),
    ?assertEqual({[{b, undefined}], []},
		strikead_yaws:params([strikead_yaws:opt(b)], Args)),
    Xf = fun(X) -> X ++ "-x" end,
    ?assertEqual({[{b, undefined}], []},
		strikead_yaws:params([strikead_yaws:opt(b, Xf)], Args)),
    ?assertEqual({[{a, "b-x"}], []},
		strikead_yaws:params([strikead_yaws:opt(a, Xf)], Args)).

convert_test() ->
    Xf = fun(X) -> X ++ "-x" end,
    ?assertEqual({[{a, "b-x"}], []},
		strikead_yaws:params([
			strikead_yaws:convert(strikead_yaws:opt(a), Xf)], mkargs())).

any_test() ->
    ?assertEqual({[{a, "b"}], []},
		strikead_yaws:params([strikead_yaws:any([
			strikead_yaws:get(x), strikead_yaws:opt(a)], "Not found")], mkargs())).

as_test() ->
    ?assertEqual({[{z, "b"}], []},
		strikead_yaws:params([strikead_yaws:as(z, strikead_yaws:any([
			strikead_yaws:get(x), strikead_yaws:opt(a)], "Not Found"))], mkargs())).

list_test() ->
	?assertEqual({[{x, ["x", "y"]}], []},
		strikead_yaws:params([strikead_yaws:list(x)], mkargs([{x, "x"}, {x, "y"}]))),
	?assertEqual({[{x, ["x"]}], []},
		strikead_yaws:params([strikead_yaws:list(x)], mkargs([{x, "x"}]))),
	?assertEqual({[{x, []}], []},
		strikead_yaws:params([strikead_yaws:list(x)], mkargs())).

parse_params_test() ->
    ?assertEqual([{a ,"b"}, {c, "d"}], strikead_yaws:parse_params(mkargs())).

params_test() ->
    ?assertEqual({[{a, "b"},{c, "d"}], []}, strikead_yaws:params([
		strikead_yaws:get(a),
		strikead_yaws:get(c)
	],mkargs())).

mkargs(Query) ->
	strikead_yaws:clear_parse_caches(),
    #arg{
        req = #http_request{method='GET', path="/test", version="HTTP/1.1"},
        querydata = string:join([atom_to_list(K) ++ "=" ++ V || {K, V} <- Query], "&")
    }.

mkargs() -> mkargs([{a,"b"}, {c,"d"}]).
