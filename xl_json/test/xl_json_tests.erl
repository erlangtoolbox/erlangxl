-module(xl_json_tests).

-include_lib("eunit/include/eunit.hrl").

to_json_test() ->
    ?assertEqual("\"\"", xl_json:to_json(<<>>)),
    ?assertEqual("\"string\"", xl_json:to_json(<<"string">>)),
    ?assertEqual("\"stri\\ng\"", xl_json:to_json(<<"stri\ng">>)),
    ?assertEqual("1", xl_json:to_json(1)),
    ?assertEqual("1.2", xl_json:to_json(1.2)),
    ?assertEqual("true", xl_json:to_json(true)),
    ?assertEqual("false", xl_json:to_json(false)),
    ?assertEqual("[]", xl_json:to_json([])),
    ?assertEqual("[1,2,\"3\",false,null]",
        xl_json:to_json([1, 2, <<"3">>, false, undefined])),
    ?assertEqual("[1,[1,2]]", xl_json:to_json([1, [1, 2]])),
    ?assertEqual("{\"a\":[1],\"b\":{\"c\":null}}",
        xl_json:to_json([{"a", [1]}, {b, [{c, undefined}]}])).

from_json_test() ->
    ?assertEqual({ok, true}, xl_json:from_json("true")),
    ?assertEqual({ok, [{a, [{b, [1, 2]}]}]},
        xl_json:from_json("{a:{b:[1,2]}}")),
    ?assertEqual({ok, [{a, undefined}]},
        xl_json:from_json("{a:null}")),
    ?assertEqual({ok, [<<"1aa">>, 1]},
        xl_json:from_json("[\"1aa\", 1]")),
    ?assertEqual({ok, [{data, []}, {keys, [<<"user1">>]}]},
        xl_json:from_json(<<"{\"keys\":[\"user1\"],\"data\":[]}">>)).

