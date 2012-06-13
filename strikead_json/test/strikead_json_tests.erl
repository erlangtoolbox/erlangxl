-module(strikead_json_tests).

-include_lib("eunit/include/eunit.hrl").

-include("rec.hrl").

to_json_test() ->
    ?assertEqual("\"\"", strikead_json:to_json(<<>>)),
    ?assertEqual("\"string\"", strikead_json:to_json(<<"string">>)),
    ?assertEqual("\"stri\\ng\"", strikead_json:to_json(<<"stri\ng">>)),
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

from_json_test() ->
	?assertEqual({ok, true}, strikead_json:from_json("true")),
	?assertEqual({ok, [{a, [{b, [1, 2]}]}]},
		strikead_json:from_json("{a:{b:[1,2]}}")),
	?assertEqual({ok, [{a, undefined}]},
		strikead_json:from_json("{a:null}")),
	?assertEqual({ok, [<<"1aa">>, 1]},
		strikead_json:from_json("[\"1aa\", 1]")),
	?assertEqual({ok, [{data,[]}, {keys, [<<"user1">>]}]},
		strikead_json:from_json(<<"{\"keys\":[\"user1\"],\"data\":[]}">>)).

