-module(xl_json_bindc).

-compile({parse_transform, do}).

-export([compile/2]).

compile(Path, Dest) ->
    Module = filename:basename(Path, filename:extension(Path)),
    HrlPath = filename:join([Dest, "include", Module ++ ".hrl"]),
    ModulePath = filename:join([Dest, "src", Module ++ ".erl"]),
    do([error_m ||
        io:format("compile ~p to ~p~n", [Path, Dest]),
        Records <- file:consult(Path),
        generate_file(HrlPath, fun(F) -> generate_records(Records, F) end),
        generate_file(ModulePath, fun(F) -> generate_module(Records, Module, F) end)
    ]).


generate_records(Records, Out) ->
    xl_lists:eforeach(fun({Name, Fields}) ->
        do([error_m ||
            Generated <- xl_lists:emap(fun(Field) -> generate_field(Field) end, Fields),
            io:format(Out, "~n-record(~s, {~n\t~s~n}).", [Name, xl_string:join(Generated, ",\n\t")])
        ])
    end, Records).



%lists
generate_field({Name, {list, string}}) -> generate_field({Name, {list, binary}});
generate_field({Name, {list, Type}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == binary;
    Type == any ->
    {ok, xl_string:format("~p = error({required, ~p}) :: [~p()]", [Name, Name, Type])};
generate_field({Name, {list, Type}}) when is_atom(Type) ->
    {ok, xl_string:format("~p = error({required, ~p}) :: [#~p{}]", [Name, Name, Type])};
generate_field({Name, {list, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = error({required, ~p})", [Name, Name])};

%lists with defaults
generate_field({Name, {list, string, Default}}) -> generate_field({Name, {list, binary, Default}});
generate_field({Name, {list, Type, Default}}) when
    is_list(Default), Type == binary;
    is_list(Default), Type == integer;
    is_list(Default), Type == float;
    is_list(Default), Type == boolean;
    is_list(Default), Type == atom;
    is_list(Default), Type == any ->
    {ok, xl_string:format("~p = ~p :: [~p()]", [Name, Default, Type])};
generate_field({Name, {list, Type, Default}}) when is_atom(Type), is_list(Default) ->
    {ok, xl_string:format("~p = ~p :: [#~p{}]", [Name, Default, Type])};
generate_field({Name, {list, {Module, Type}, Default}}) when is_atom(Module), is_atom(Type), is_list(Default) ->
    {ok, xl_string:format("~p = ~p", [Name, Default])};

%options
generate_field({Name, {option, string}}) -> generate_field({Name, {option, binary}});
generate_field({Name, {option, Type}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == binary;
    Type == any ->
    {ok, xl_string:format("~p :: option_m:monad(~p())", [Name, Type])};
generate_field({Name, {option, Type}}) when is_atom(Type) ->
    {ok, xl_string:format("~p :: option_m:monad(#~p{})", [Name, Type])};
generate_field({Name, {option, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p", [Name])};

%options with defaults
generate_field({Name, {option, string, Default}}) -> generate_field({Name, {option, binary, Default}});
generate_field({Name, {option, Type, Default}}) when
    is_binary(Default), Type == binary;
    is_integer(Default), Type == integer;
    is_float(Default), Type == float;
    is_atom(Default), Type == atom;
    Default == true, Type == boolean;
    Default == false, Type == boolean;
    is_list(Default), Type == any ->
    {ok, xl_string:format("~p = {ok, ~p} :: option_m:monad(~p())", [Name, Default, Type])};

%primitives with defaults
generate_field({Name, {Type, Default}}) when
    is_binary(Default), Type == binary;
    is_integer(Default), Type == integer;
    is_float(Default), Type == float;
    is_atom(Default), Type == atom;
    Default == true, Type == boolean;
    Default == false, Type == boolean;
    is_list(Default), Type == any ->
    {ok, xl_string:format("~p = ~p :: ~p()", [Name, Default, Type])};
generate_field({Name, {string, Default}}) ->
    generate_field({Name, {binary, Default}});

%primitives
generate_field({Name, string}) -> generate_field({Name, binary});
generate_field({Name, Type}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == binary;
    Type == any ->
    {ok, xl_string:format("~p = error({required, ~p}) :: ~p()", [Name, Name, Type])};
generate_field({Name, Type}) when is_atom(Type) ->
    {ok, xl_string:format("~p = error({required, ~p}) :: #~p{}", [Name, Name, Type])};
generate_field({Name, {Type, undefined}}) when is_atom(Type) ->
    {ok, xl_string:format("~p :: #~p{}", [Name, Type])};
generate_field({Name, {Module, Type}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = error({required, ~p}) ", [Name, Name])};
generate_field({Name, {{Module, Type}, undefined}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p", [Name])};

%wtf
generate_field(D) -> {error, {dont_understand, D}}.

generate_file(Path, Generate) ->
    io:format("Generate ~s~n", [Path]),
    do([error_m ||
        filelib:ensure_dir(Path),
        Out <- file:open(Path, [write]),
        file:write(Out, "%% Generated by " ++ atom_to_list(?MODULE) ++ "\n"),
        try
            Generate(Out)
        after
            file:close(Out)
        end
    ]).

generate_module(Records, Name, Out) ->
    do([error_m ||
        file:write(Out, "-module(" ++ Name ++ ").\n\n"),
        file:write(Out, "-include(\"" ++ Name ++ ".hrl\").\n\n"),
        file:write(Out, "-export([to_json/1, from_json/2, from_json_/2]).\n\n"),
        file:write(Out, "to_json(undefined) -> \"null\";\n\n"),
        file:write(Out, "to_json({ok, X}) -> to_json(X);\n\n"),
        file:write(Out, "to_json(L) when is_list(L) -> \"[\" ++ string:join([to_json(R) || R <- L], \",\") ++ \"]\";\n\n"),
        generate_to_json(Records, Out),
        file:write(Out, "from_json(Json, Record) when is_list(Json); is_binary(Json) ->\n"
        "\tcase ktj_parse:parse(Json) of\n"
        "\t\t{L, _, _} when is_list(L)->\n"
        "\t\t\ttry\n"
        "\t\t\t\t{ok, [from_json_(R, Record) || R <- L]}\n"
        "\t\t\tcatch\n"
        "\t\t\t\terror:X -> {error, X}\n"
        "\t\t\tend;\n"
        "\t\t{R, _, _} ->\n"
        "\t\t\ttry\n"
        "\t\t\t\t{ok, from_json_(R, Record)}\n"
        "\t\t\tcatch\n"
        "\t\t\t\terror:X -> {error, X}\n"
        "\t\t\tend;\n"
        "\t\tX -> X\n"
        "end.\n\n"),
        file:write(Out, "from_json_(undefined, _Record)  -> undefined;\n\n"),
        generate_from_json(Records, Out)
    ]).

generate_to_json(Records, Out) ->
    do([error_m ||
        Functions <- xl_lists:emap(fun({RecordName, Fields}) ->
            do([error_m ||
                Generated <- xl_lists:emap(fun(Field) -> generate_to_json_field(RecordName, Field) end, Fields),
                return(
                    xl_string:format(
                        "to_json(R=#~p{}) ->\n\txl_string:join([\"{\",\n~s\n\t\"}\"])", [RecordName,
                            xl_string:join([xl_string:format("\t\t~s,", [F]) || F <- Generated], " \",\",\n")
                        ]
                    )
                )
            ])
        end, Records),
        file:write(Out, xl_string:join(Functions, ";\n") ++ ".\n\n")
    ]).

generate_to_json_field(RecordName, {Name, Type}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    {ok, xl_string:format("\"\\\"~p\\\":\", xl_json:to_json(R#~p.~p)", [Name, RecordName, Name])};
generate_to_json_field(RecordName, {Name, {Type, _Default}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    generate_to_json_field(RecordName, {Name, Type});
generate_to_json_field(RecordName, {Name, {list, Type}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    generate_to_json_field(RecordName, {Name, Type});
generate_to_json_field(RecordName, {Name, {list, Type, _Default}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    generate_to_json_field(RecordName, {Name, Type});
generate_to_json_field(RecordName, {Name, {option, Type}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    generate_to_json_field(RecordName, {Name, Type});
generate_to_json_field(RecordName, {Name, {option, Type, _Default}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    generate_to_json_field(RecordName, {Name, Type});

generate_to_json_field(RecordName, {Name, {option, Type}}) when is_atom(Type) ->
    {ok, xl_string:format("\"\\\"~p\\\":\", to_json(R#~p.~p)", [Name, RecordName, Name])};
generate_to_json_field(RecordName, {Name, {option, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("\"\\\"~p\\\":\", ~p:to_json(R#~p.~p)", [Name, Module, RecordName, Name])};

generate_to_json_field(RecordName, {Name, {list, Type}}) when is_atom(Type) ->
    {ok, xl_string:format("\"\\\"~p\\\":\", \"[\" ++ string:join([to_json(X)||X <- R#~p.~p], \",\") ++ \"]\"", [Name, RecordName, Name])};
generate_to_json_field(RecordName, {Name, {list, Type, _Default}}) when is_atom(Type) ->
    generate_to_json_field(RecordName, {Name, {list, Type}});

generate_to_json_field(RecordName, {Name, {list, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("\"\\\"~p\\\":\", \"[\" ++ string:join([~p:to_json(X)||X <- R#~p.~p], \",\") ++ \"]\"", [Name, Module, RecordName, Name])};
generate_to_json_field(RecordName, {Name, {list, {Module, Type}, _Default}}) when is_atom(Module), is_atom(Type) ->
    generate_to_json_field(RecordName, {Name, {list, {Module, Type}}});

generate_to_json_field(RecordName, {Name, Type}) when is_atom(Type) ->
    {ok, xl_string:format("\"\\\"~p\\\":\", to_json(R#~p.~p)", [Name, RecordName, Name])};
generate_to_json_field(RecordName, {Name, {Type, undefined}}) when is_atom(Type) ->
    generate_to_json_field(RecordName, {Name, Type});
generate_to_json_field(RecordName, {Name, {Module, Type}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("\"\\\"~p\\\":\", ~p:to_json(R#~p.~p)", [Name, Module, RecordName, Name])};
generate_to_json_field(RecordName, {Name, {{Module, Type}, undefined}}) when is_atom(Module), is_atom(Type) ->
    generate_to_json_field(RecordName, {Name, {Module, Type}});

generate_to_json_field(_RecordName, Field) ->
    {error, {dont_understand, Field}}.

generate_from_json(Records, Out) ->
    do([error_m ||
        Functions <- xl_lists:emap(fun({RecordName, Fields}) ->
            do([error_m ||
                Generated <- xl_lists:emap(fun(Field) -> generate_from_json_field(Field) end, Fields),
                return(
                    xl_string:format(
                        "from_json_(J, ~p) ->\n\t#~p{\n~s\n\t}",
                        [RecordName, RecordName, xl_string:join([xl_string:format("\t\t~s", [F]) || F <- Generated], ",\n")]
                    )
                )
            ])
        end, Records),
        file:write(Out, xl_string:join(Functions, ";\n") ++ ".\n\n")
    ]).

generate_from_json_field({Name, Type}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    {ok, xl_string:format("~p = xl_json:ktuo_find(~p, J, ~p, ~p)", [Name, Name, undefined, Type])};
generate_from_json_field({Name, Qualified = {list, Type}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    {ok, xl_string:format("~p = xl_json:ktuo_find(~p, J, ~p, ~p)", [Name, Name, [], Qualified])};
generate_from_json_field({Name, Qualified = {option, Type}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    {ok, xl_string:format("~p = xl_json:ktuo_find(~p, J, ~p, ~p)", [Name, Name, undefined, Qualified])};
generate_from_json_field({Name, {option, Type, Default}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    {ok, xl_string:format("~p = xl_json:ktuo_find(~p, J, ~p, ~p)", [Name, Name, Default, {option, Type}])};
generate_from_json_field({Name, {Type, Default}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    {ok, xl_string:format("~p = xl_json:ktuo_find(~p, J, ~p, ~p)", [Name, Name, Default, Type])};
generate_from_json_field({Name, {list, Type, Default}}) when
    Type == integer;
    Type == float;
    Type == boolean;
    Type == atom;
    Type == string;
    Type == any ->
    {ok, xl_string:format("~p = xl_json:ktuo_find(~p, J, ~p, ~p)", [Name, Name, Default, {list, Type}])};

generate_from_json_field({Name, {list, Qualified = {Module, Type}, Default}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = [~p:from_json_(O, ~p) || O <- xl_json:ktuo_find(~p, J, ~p, ~p)]", [Name, Module, Type, Name, Default, Qualified])};

generate_from_json_field({Name, {list, Type, Default}}) when is_atom(Type) ->
    {ok, xl_string:format("~p = [from_json_(O, ~p) || O <- xl_json:ktuo_find(~p, J, ~p, ~p)]", [Name, Type, Name, Default, {list, Type}])};

generate_from_json_field({Name, Qualified = {list, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = [~p:from_json_(O, ~p) || O <- xl_json:ktuo_find(~p, J, ~p, ~p)]", [Name, Module, Type, Name, [], Qualified])};

generate_from_json_field({Name, Qualified = {list, Type}}) when is_atom(Type) ->
    {ok, xl_string:format("~p = [from_json_(O, ~p) || O <- xl_json:ktuo_find(~p, J, ~p, ~p)]", [Name, Type, Name, [], Qualified])};

generate_from_json_field({Name, {option, Type}}) when is_atom(Type) ->
    {ok, xl_string:format("~p = case from_json_(xl_json:ktuo_find(~p, J, ~p, ~p), ~p) of undefined -> undefined; X -> {ok, X} end", [Name, Name, undefined, Type, Type])};

generate_from_json_field({Name, {option, Qualified = {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = case ~p:from_json_(xl_json:ktuo_find(~p, J, ~p, ~p), ~p) of undefined -> undefined; X -> {ok, X} end", [Name, Module, Name, undefined, Qualified, Type])};

generate_from_json_field({Name, {Type, undefined}}) when is_atom(Type) ->
    generate_from_json_field({Name, Type});
generate_from_json_field({Name, Type}) when is_atom(Type) ->
    {ok, xl_string:format("~p = from_json_(xl_json:ktuo_find(~p, J, ~p, ~p), ~p)", [Name, Name, undefined, Type, Type])};

generate_from_json_field({Name, {{Module, Type}, undefined}}) when is_atom(Module), is_atom(Type) ->
    generate_from_json_field({Name, {Module, Type}});
generate_from_json_field({Name, Qualified = {Module, Type}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = ~p:from_json_(xl_json:ktuo_find(~p, J, ~p, ~p), ~p)", [Name, Module, Name, undefined, Qualified, Type])};


generate_from_json_field(Field) -> {error, {dont_understand, Field}}.
