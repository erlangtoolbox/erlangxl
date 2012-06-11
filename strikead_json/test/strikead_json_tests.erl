-module(strikead_json_tests).

-include_lib("eunit/include/eunit.hrl").

-include("rec.hrl").

to_json_test() ->
    ?assertEqual("\"string\"", strikead_json:to_json(<<"string">>)),
    ?assertEqual("1", strikead_json:to_json(1)),
    ?assertEqual("1.2", strikead_json:to_json(1.2)),
    ?assertEqual("true", strikead_json:to_json(true)),
    ?assertEqual("false", strikead_json:to_json(false)),
    ?assertEqual("[]", strikead_json:to_json([])),
    ?assertEqual("[1,2,\"3\",false,null]",
		strikead_json:to_json([1, 2, <<"3">>, false, undefined])),
    ?assertEqual("[1,[1,2]]", strikead_json:to_json([1, [1, 2]])),
    ?assertEqual("{\"a\":[1],\"b\":{\"c\":null}}",
		strikead_json:to_json([{"a", [1]}, {b, [{c, undefined}]}])).

