-module(alltypes_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_stdlib/include/xl_eunit.hrl").

-include("alltypes.hrl").

primitives_test() ->
    P = #primitives{
        integer = 1,
        float = 1.1,
        string = <<"1">>,
        atom = atom,
        boolean = true,
        record = undefined,
        record_qualified = undefined,
        any = [{a, 1}]
    },
    Json = "{\"integer\":1,\"integer_undef\":null,\"integer_def\":1,\"float\":1.1,\"float_undef\":null,\"float_def\":1.0,\"boolean\":true,\"boolean_def_true\":true,\"boolean_def_false\":false,\"atom\":\"atom\",\"atom_def\":\"a\",\"string\":\"1\",\"string_undef\":null,\"string_def\":\"a\",\"record\":null,\"record_def\":null,\"record_qualified\":null,\"record_qualified_def\":null,\"any\":{\"a\":1},\"any_undef\":null,\"any_def\":{\"a\":\"b\"}}",
    ?assertEquals(Json, alltypes:to_json(P)),
    ?assertEquals({ok, P}, alltypes:from_json(alltypes:to_json(P), primitives)).

options_test() ->
    P = #options{
        integer = {ok, 1},
        float = {ok, 1.1},
        string = {ok, <<"1">>},
        atom = {ok, atom},
        boolean = {ok, true},
        record = undefined,
        record_qualified = undefined,
        any = {ok, [{a, 1}]}
    },
    Json = "{\"integer\":1,\"integer_def\":1,\"float\":1.1,\"float_def\":1.0,\"boolean\":true,\"boolean_def\":true,\"atom\":\"atom\",\"atom_def\":\"a\",\"string\":\"1\",\"string_def\":\"a\",\"record\":null,\"record_qualified\":null,\"any\":{\"a\":1},\"any_def\":{\"a\":\"b\"}}",
    ?assertEquals(Json, alltypes:to_json(P)),
    ?assertEquals({ok, P}, alltypes:from_json(alltypes:to_json(P), options)).

lists_test() ->
    P = #lists{
        integer = [1, 2],
        float = [1.1, 2.2],
        string = [<<"a">>, <<"b">>],
        atom = [a, b],
        boolean = [true, false],
        record = [],
        record_qualified = [],
        any = [[{a, 1}]]
    },
    Json = "{\"integer\":[1,2],\"integer_def\":[1,2],\"float\":[1.1,2.2],\"float_def\":[1.0,1.2],\"boolean\":[true,false],\"boolean_def\":[true],\"atom\":[\"a\",\"b\"],\"atom_def\":[\"a\",\"b\"],\"string\":[\"a\",\"b\"],\"string_def\":[\"a\"],\"record\":[],\"record_def\":[],\"record_qualified\":[],\"record_qualified_def\":[],\"any\":[{\"a\":1}],\"any_def\":[{\"a\":\"b\"}]}",
    ?assertEquals(Json, alltypes:to_json(P)),
    ?assertEquals({ok, P}, alltypes:from_json(alltypes:to_json(P), lists)).


qualified_test() ->
    P = #primitives{
        integer = 1,
        float = 1.1,
        string = <<"1">>,
        atom = atom,
        boolean = true,
        record = undefined,
        record_qualified = undefined,
        any = [{a, 1}]
    },
    Q = #qualified{prim = P, prim_opt = {ok, P}, list_prim = [P, P], list_prim_def = [P, P]},
    ?assertEquals({ok, Q}, alltypes:from_json(alltypes:to_json(Q), qualified)).

list_test() ->
    P = #primitives{
        integer = 1,
        float = 1.1,
        string = <<"1">>,
        atom = atom,
        boolean = true,
        record = undefined,
        record_qualified = undefined,
        any = [{a, 1}]
    },
    L = [P, P, P],
    ?assertEqual({ok, L}, alltypes:from_json(alltypes:to_json(L), primitives)).

undefined_options_test() ->
    P = #options{},
    ?assertEquals({ok, P}, alltypes:from_json("{}", options)).
