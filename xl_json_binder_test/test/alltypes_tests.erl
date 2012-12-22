-module(alltypes_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_stdlib/include/xl_eunit.hrl").

-include("alltypes.hrl").

primitives_proplist_test() ->
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
    Proplist = [
        {integer, 1},
        {float, 1.1},
        {string, <<"1">>},
        {atom, atom},
        {boolean, true},
        {record, undefined},
        {record_qualified, undefined},
        {any, [{a, 1}]}
    ],
    ?assertEquals({ok, P}, alltypes:from_proplist(Proplist, primitives)).

primitives_proplist_binaries_cast_test() ->
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
    Proplist = [
        {integer, <<"1">>},
        {float, <<"1.1">>},
        {string, <<"1">>},
        {atom, <<"atom">>},
        {boolean, <<"true">>},
        {record, undefined},
        {record_qualified, undefined},
        {any, [{a, 1}]}
    ],
    ?assertEquals({ok, P}, alltypes:from_proplist(Proplist, primitives)).

integer_to_float_cast_test() ->
    P = #primitives{
        integer = 1,
        float = 1.0,
        string = <<"1">>,
        atom = atom,
        boolean = true,
        record = undefined,
        record_qualified = undefined,
        any = [{a, 1}]
    },
    Proplist = [
        {integer, <<"1">>},
        {float, <<"1">>},
        {string, <<"1">>},
        {atom, <<"atom">>},
        {boolean, <<"true">>},
        {record, undefined},
        {record_qualified, undefined},
        {any, [{a, 1}]}
    ],
    ?assertEquals({ok, P}, alltypes:from_proplist(Proplist, primitives)),
    Json = "{\"integer\":1,\"integer_undef\":null,\"integer_def\":1,\"float\":1,\"float_undef\":null,\"float_def\":1.0,\"boolean\":true,\"boolean_def_true\":true,\"boolean_def_false\":false,\"atom\":\"atom\",\"atom_def\":\"a\",\"string\":\"1\",\"string_undef\":null,\"string_def\":\"a\",\"record\":null,\"record_def\":null,\"record_qualified\":null,\"record_qualified_def\":null,\"any\":{\"a\":1},\"any_undef\":null,\"any_def\":{\"a\":\"b\"}}",
    ?assertEquals({ok, P}, alltypes:from_json(Json, primitives)).

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

options_proplist_test() ->
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
    Proplist = [
        {integer, 1},
        {float, 1.1},
        {string, <<"1">>},
        {atom, atom},
        {boolean, true},
        {any, [{a, 1}]}
    ],

    ?assertEquals({ok, P}, alltypes:from_proplist(Proplist, options)).

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

lists_integer_to_float_test() ->
    P = #lists{
        integer = [1, 2],
        float = [1.0, 2.0],
        string = [<<"a">>, <<"b">>],
        atom = [a, b],
        boolean = [true, false],
        record = [],
        record_qualified = [],
        any = [[{a, 1}]]
    },
    Json = "{\"integer\":[1,2],\"integer_def\":[1,2],\"float\":[1,2],\"float_def\":[1.0,1.2],\"boolean\":[true,false],\"boolean_def\":[true],\"atom\":[\"a\",\"b\"],\"atom_def\":[\"a\",\"b\"],\"string\":[\"a\",\"b\"],\"string_def\":[\"a\"],\"record\":[],\"record_def\":[],\"record_qualified\":[],\"record_qualified_def\":[],\"any\":[{\"a\":1}],\"any_def\":[{\"a\":\"b\"}]}",
    ?assertEquals({ok, P}, alltypes:from_json(Json, lists)).


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

required_test() ->
    Asserts = [
        {integer, "{}"},
        {float, "{\"integer\":1}"},
        {boolean, "{\"integer\":1,\"float\":1.1}"},
        {atom, "{\"integer\":1,\"float\":1.1,\"boolean\":true}"},
        {string, "{\"integer\":1,\"float\":1.1,\"boolean\":true,\"atom\":\"atom\"}"},
        {record, "{\"integer\":1,\"float\":1.1,\"boolean\":true,\"atom\":\"atom\",\"string\":\"1\"}"},
        {record_qualified, "{\"integer\":1,\"float\":1.1,\"boolean\":true,\"atom\":\"atom\",\"string\":\"1\",\"record\":null}"},
        {any, "{\"integer\":1,\"float\":1.1,\"boolean\":true,\"atom\":\"atom\",\"string\":\"1\",\"record\":null,\"record_qualified\":null}"}
    ],
    lists:foreach(fun({Name, Json}) ->
        ?assertEquals({error, {required, Name}}, alltypes:from_json(Json, primitives))
    end, Asserts),
    ListAsserts = [
        {integer, "{}"},
        {float, "{\"integer\":[]}"},
        {boolean, "{\"integer\":[],\"float\":[]}"},
        {atom, "{\"integer\":[],\"float\":[],\"boolean\":[]}"},
        {string, "{\"integer\":[],\"float\":[],\"boolean\":[],\"atom\":[]}"},
        {record, "{\"integer\":[],\"float\":[],\"boolean\":[],\"atom\":[],\"string\":[]}"},
        {record_qualified, "{\"integer\":[],\"float\":[],\"boolean\":[],\"atom\":[],\"string\":[],\"record\":[]}"},
        {any, "{\"integer\":[],\"float\":[],\"boolean\":[],\"atom\":[],\"string\":[],\"record\":[],\"record_qualified\":[]}"}
    ],
    lists:foreach(fun({Name, Json}) ->
        ?assertEquals({error, {required, Name}}, alltypes:from_json(Json, lists))
    end, ListAsserts),
    NullListAsserts = [
        {any, "{\"integer\":null,\"float\":[],\"boolean\":[],\"atom\":[],\"string\":[],\"record\":[],\"record_qualified\":[]}"}
    ],
    lists:foreach(fun({Name, Json}) ->
        ?assertEquals({error, {required, Name}}, alltypes:from_json(Json, lists))
    end, NullListAsserts).

primitive_enums_test() ->
    P = #primitive_enums{
        integer = 1,
        float = 1.1,
        string = <<"a">>,
        atom = a
    },
    Json = "{\"integer\":1,\"integer_undef\":null,\"integer_def\":1,\"float\":1.1,\"float_undef\":null,\"float_def\":1.1,\"atom\":\"a\",\"atom_def\":\"a\",\"string\":\"a\",\"string_undef\":null,\"string_def\":\"a\"}",
    ?assertEquals(Json, alltypes:to_json(P)),
    ?assertEquals({ok, P}, alltypes:from_json(alltypes:to_json(P), primitive_enums)).


primitive_enum_validation_test() ->
    Asserts = [
        {0, "{\"integer\":0}"},
        {1.0, "{\"integer\":1,\"float\":1.0}"},
        {x, "{\"integer\":1,\"float\":1.1,\"atom\":\"x\"}"},
        {<<"x">>, "{\"integer\":1,\"float\":1.1,\"atom\":\"a\",\"string\":\"x\"}"}
    ],
    lists:foreach(fun({Value, Json}) ->
        ?assertEquals({error, {illegal_enum_value, Value}}, alltypes:from_json(Json, primitive_enums))
    end, Asserts).

list_enums_test() ->
    P = #list_enums{
        integer = [1],
        float = [1.1],
        string = [<<"a">>],
        atom = [a]
    },
    Json = "{\"integer\":[1],\"integer_def\":[1],\"float\":[1.1],\"float_def\":[1.1],\"atom\":[\"a\"],\"atom_def\":[\"a\"],\"string\":[\"a\"],\"string_def\":[\"a\"]}",
    ?assertEquals(Json, alltypes:to_json(P)),
    ?assertEquals({ok, P}, alltypes:from_json(alltypes:to_json(P), list_enums)).

list_enum_validation_test() ->
    Asserts = [
        {0, "{\"integer\":[0]}"},
        {1.0, "{\"integer\":[1],\"float\":[1.0]}"},
        {x, "{\"integer\":[1],\"float\":[1.1],\"atom\":[\"x\"]}"},
        {<<"x">>, "{\"integer\":[1],\"float\":[1.1],\"atom\":[\"a\"],\"string\":[\"x\"]}"}
    ],
    lists:foreach(fun({Value, Json}) ->
        ?assertEquals({error, {illegal_enum_value, Value}}, alltypes:from_json(Json, list_enums))
    end, Asserts).

list_content_validation_test() ->
    Asserts = [
        {[1], "{\"integer\":[],\"float\":[],\"boolean\":[],\"atom\":[],\"string\":[1],\"record\":[],\"record_qualified\":[]}"}
    ],
    lists:foreach(fun({Value, Json}) ->
        ?assertEquals({error, {illegal_array_value, Value}}, alltypes:from_json(Json, lists))
    end, Asserts).

