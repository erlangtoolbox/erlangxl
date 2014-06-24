%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(xl_json_bindc).

-compile({parse_transform, do}).

-export([compile/2]).

%runtime
-export([cast/4, cast/3, check_required/1, format/1]).

-define(JSON_API, xl_json_jiffy).

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

-define(is_primitive_type(Type), Type == integer; Type == float; Type == boolean; Type == atom; Type == binary; Type == string; Type == any).
-define(is_primitive_type(Type, Default),
    is_binary(Default), Type == string;
    Default == undefined, Type == string;
    is_binary(Default), Type == binary;
    Default == undefined, Type == binary;
    is_integer(Default), Type == integer;
    Default == undefined, Type == integer;
    is_float(Default), Type == float;
    Default == undefined, Type == float;
    is_atom(Default), Type == atom;
    Default == true, Type == boolean;
    Default == false, Type == boolean;
    is_list(Default), Type == any;
    Default == undefined, Type == any
).

%private
generate_field({Name, private}) ->
    {ok, xl_string:format("~p", [Name])};
generate_field({Name, {private, Default}}) ->
    {ok, xl_string:format("~p = ~p", [Name, Default])};

%lists
generate_field({Name, {list, Type}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = error({required, ~p}) :: [~p()]", [Name, Name, Type])};
generate_field({Name, {list, Type}}) when is_atom(Type) ->
    {ok, xl_string:format("~p = error({required, ~p}) :: [#~p{}]", [Name, Name, Type])};
generate_field({Name, {list, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = error({required, ~p})", [Name, Name])};

%lists with defaults
generate_field({Name, {list, Type, Default}}) when is_list(Default), ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = ~p :: [~p()]", [Name, Default, Type])};
generate_field({Name, {list, Type, Default}}) when is_atom(Type), is_list(Default) ->
    {ok, xl_string:format("~p = ~p :: [#~p{}]", [Name, Default, Type])};
generate_field({Name, {list, {Module, Type}, Default}}) when is_atom(Module), is_atom(Type), is_list(Default) ->
    {ok, xl_string:format("~p = ~p", [Name, Default])};


%dicts with defaults
generate_field({Name, {dict, Type, {_Key, []}}}) when is_atom(Type) ->
    {ok, xl_string:format("~p = gb_trees:empty() :: gb_tree()", [Name])};
generate_field({Name, {dict, {Module, Type}, {_Key, []}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = gb_trees:empty()", [Name])};

%dicts
generate_field({Name, {dict, Type, _Key}}) when is_atom(Type) ->
    {ok, xl_string:format("~p = error({required, ~p}) :: gb_tree()", [Name, Name])};
generate_field({Name, {dict, {Module, Type}, _Key}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = error({required, ~p}) :: gb_tree()", [Name, Name])};

%options
generate_field({Name, {option, Type}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p :: option_m:monad(~p())", [Name, Type])};
generate_field({Name, {option, Type}}) when is_atom(Type) ->
    {ok, xl_string:format("~p :: option_m:monad(#~p{})", [Name, Type])};
generate_field({Name, {option, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p", [Name])};

%options with defaults
generate_field({Name, {option, Type, Default}}) when ?is_primitive_type(Type, Default) ->
    {ok, xl_string:format("~p = {ok, ~p} :: option_m:monad(~p())", [Name, Default, Type])};

%enums
generate_field({Name, {enum, Type, _Enum}}) -> generate_field({Name, Type});

%either
generate_field({Name, {either, _Types}}) ->
    {ok, xl_string:format("~p = error({required, ~p})", [Name, Name])};

%primitives with defaults
generate_field({Name, {Type, Default}}) when ?is_primitive_type(Type, Default) ->
    {ok, xl_string:format("~p = ~p :: ~p()", [Name, Default, Type])};
generate_field({Name, {string, Default}}) -> generate_field({Name, {binary, Default}});

%primitives
%% generate_field({Name, string}) -> generate_field({Name, binary});
generate_field({Name, Type}) when ?is_primitive_type(Type) ->
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
generate_field(D) -> {error, {gen_record, dont_understand, D}}.

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
        io:format(Out, "-define(JSON_API, ~s).\n\n", [?JSON_API]),
        file:write(Out, "-export([to_json/1, from_json/2, from_json_/2, from_proplist/2, from_proplist_/2]).\n\n"),
        file:write(Out, "to_json(undefined) -> <<\"null\">>;\n\n"),
        file:write(Out, "to_json({ok, X}) -> to_json(X);\n\n"),
        file:write(Out, "to_json(L) when is_list(L) -> xl_string:flatten([<<\"[\">>, xl_lists:disperse([to_json(R) || R <- L], <<\",\">>), <<\"]\">>]);\n\n"),
        generate_to_json(Records, Out),
        file:write(Out, "from_json(Json, Type) ->\n"
        "\tcase ?JSON_API:from_json(Json) of\n"
        "\t\t{ok, List} when is_list(List)->\n"
        "\t\t\ttry\n"
        "\t\t\t\t{ok, [from_json_(R, Type) || R <- List]}\n"
        "\t\t\tcatch\n"
        "\t\t\t\terror:X -> {error, X}\n"
        "\t\t\tend;\n"
        "\t\t{ok, Document} ->\n"
        "\t\t\ttry\n"
        "\t\t\t\t{ok, from_json_(Document, Type)}\n"
        "\t\t\tcatch\n"
        "\t\t\t\terror:X -> {error, X}\n"
        "\t\t\tend;\n"
        "\t\tError -> Error\n"
        "end.\n\n"),
        file:write(Out, "from_json_(undefined, _Type)  -> undefined;\n\n"),
        generate_from_json(Records, Out),
        file:write(Out, "from_proplist(Proplist, Type) -> {ok, from_proplist_(Proplist, Type)}.\n\n"),
        file:write(Out, "from_proplist_(undefined, _Type)  -> undefined;\n\n"),
        generate_from_proplist(Records, Out)
    ]).

serializable_fields(Fields) ->
    lists:filter(fun
        ({_, private}) -> false;
        ({_, {private, _}}) -> false;
        (_) -> true
    end, Fields).

generate_to_json(Records, Out) ->
    do([error_m ||
        Functions <- xl_lists:emap(fun({RecordName, Fields}) ->
            do([error_m ||
                Generated <- xl_lists:emap(fun(Field) ->
                    generate_to_json_field(RecordName, Field) end, serializable_fields(Fields)),
                return(
                    xl_string:format(
                        "to_json(R=#~p{}) ->\n\txl_string:flatten([<<\"{\">>,\n\txl_lists:disperse(xl_lists:mapfilter(fun({false, undefined, _}) -> undefined;({_, _, X}) -> {ok, X} end, [\n~s\n\t]), <<\",\">>),\n\t<<\"}\">>])", [
                            RecordName,
                            xl_string:join([
                                xl_string:format("\t\t{~p, R#~p.~p, [<<\"\\\"\">>, ~p, <<\"\\\":\">>, ~s]}", [Required, RecordName, Name, Name, V])
                                || {Required, Name, V} <- Generated
                            ], ",\n")
                        ]
                    )
                )
            ])
        end, Records),
        file:write(Out, xl_string:join(Functions, ";\n") ++ ".\n\n")
    ]).

generate_to_json_field(RecordName, {Name, {enum, Type, _Enumeration}}) ->
    generate_to_json_field(RecordName, {Name, Type});
generate_to_json_field(RecordName, {Name, {either, _Types}}) ->
    {ok, {true, Name, xl_string:format("xl_json_bindc:format(R#~p.~p)", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, Type}) when ?is_primitive_type(Type) ->
    {ok, {true, Name, xl_string:format("xl_json_bindc:format(R#~p.~p)", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {Type, _Default}}) when ?is_primitive_type(Type) ->
    {ok, {false, Name, xl_string:format("xl_json_bindc:format(R#~p.~p)", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {list, Type}}) when ?is_primitive_type(Type) ->
    {ok, {true, Name, xl_string:format("xl_json_bindc:format(R#~p.~p)", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {list, Type, _Default}}) when ?is_primitive_type(Type) ->
    {ok, {false, Name, xl_string:format("xl_json_bindc:format(R#~p.~p)", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {option, Type}}) when ?is_primitive_type(Type) ->
    {ok, {true, Name, xl_string:format("xl_json_bindc:format(R#~p.~p)", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {option, Type, _Default}}) when ?is_primitive_type(Type) ->
    {ok, {false, Name, xl_string:format("xl_json_bindc:format(R#~p.~p)", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {option, Type}}) when is_atom(Type) ->
    {ok, {true, Name, xl_string:format("to_json(R#~p.~p)", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {option, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, {true, Name, xl_string:format("~p:to_json(R#~p.~p)", [Module, RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {list, Type}}) when is_atom(Type) ->
    {ok, {true, Name, xl_string:format("[<<\"[\">>, xl_lists:disperse([to_json(X)||X <- R#~p.~p], <<\",\">>), <<\"]\">>]", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {list, Type, _Default}}) when is_atom(Type) ->
    {ok, {false, Name, xl_string:format("[<<\"[\">>, xl_lists:disperse([to_json(X)||X <- R#~p.~p], <<\",\">>), <<\"]\">>]", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {list, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, {true, Name, xl_string:format("[<<\"[\">>, xl_lists:disperse([~p:to_json(X)||X <- R#~p.~p], <<\",\">>), <<\"]\">>]", [Module, RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {list, {Module, Type}, _Default}}) when is_atom(Module), is_atom(Type) ->
    {ok, {false, Name, xl_string:format("[<<\"[\">>, xl_lists:disperse([~p:to_json(X)||X <- R#~p.~p], <<\",\">>), <<\"]\">>]", [Module, RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {dict, Type, _Opts}}) when is_atom(Type) ->
    {ok, {true, Name, xl_string:format("[<<\"[\">>, xl_lists:disperse([to_json(X)||X <- gb_trees:values(R#~p.~p)], <<\",\">>), <<\"]\">>]", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {dict, {Module, Type}, _Opts}}) when is_atom(Module), is_atom(Type) ->
    {ok, {true, Name, xl_string:format("[<<\"[\">>, xl_lists:disperse([~p:to_json(X)||X <- gb_trees:values(R#~p.~p)], <<\",\">>), <<\"]\">>]", [Module, RecordName, Name])}};
generate_to_json_field(RecordName, {Name, Type}) when is_atom(Type) ->
    {ok, {true, Name, xl_string:format("to_json(R#~p.~p)", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {Type, undefined}}) when is_atom(Type) ->
    {ok, {false, Name, xl_string:format("to_json(R#~p.~p)", [RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {Module, Type}}) when is_atom(Module), is_atom(Type) ->
    {ok, {true, Name, xl_string:format("~p:to_json(R#~p.~p)", [Module, RecordName, Name])}};
generate_to_json_field(RecordName, {Name, {{Module, Type}, undefined}}) when is_atom(Module), is_atom(Type) ->
    {ok, {false, Name, xl_string:format("~p:to_json(R#~p.~p)", [Module, RecordName, Name])}};
generate_to_json_field(_RecordName, Field) -> {error, {gen_to_json, dont_understand, Field}}.

generate_from_json(Records, Out) ->
    do([error_m ||
        Functions <- xl_lists:emap(fun({RecordName, Fields}) ->
            do([error_m ||
                Generated <- xl_lists:emap(fun(Field) ->
                    generate_from_json_field(RecordName, Field) end, serializable_fields(Fields)),
                return(
                    xl_string:format("from_json_(J, ~p) ->\n\tR = ?JSON_API:bind(fun\n\t\t~s\n\t;(_, _, T)-> T end, ~p, J),\n\txl_json_bindc:check_required(R),\n\tR", [
                        RecordName,
                        xl_string:join([xl_string:format("\t\t~s", [F]) || F <- Generated], ";\n"),
                        list_to_tuple([RecordName | lists:map(fun default_or_required/1, Fields)])
                    ])
                )
            ])
        end, Records),
        file:write(Out, xl_string:join(Functions, ";\n") ++ ".\n\n")
    ]).

default_or_required({_Name, private}) -> undefined;
default_or_required({_Name, {private, Default}}) -> Default;
default_or_required({Name, {enum, Type, _Enumeration}}) when ?is_primitive_type(Type) -> {required, Name};
default_or_required({Name, {either, _Types}}) -> {required, Name};
default_or_required({_Name, {enum, {Type, Default}, _Enumeration}}) when ?is_primitive_type(Type) -> Default;
default_or_required({Name, Type}) when ?is_primitive_type(Type) -> {required, Name};
default_or_required({Name, {enum, {list, Type}, _Enumeration}}) when ?is_primitive_type(Type) -> {required, Name};
default_or_required({Name, {list, Type}}) when ?is_primitive_type(Type) -> {required, Name};
default_or_required({_Name, {option, Type}}) when ?is_primitive_type(Type) -> undefined;
default_or_required({_Name, {option, Type, Default}}) when ?is_primitive_type(Type) -> {ok, Default};
default_or_required({_Name, {Type, Default}}) when ?is_primitive_type(Type) -> Default;
default_or_required({_Name, {list, Type, Default}}) when ?is_primitive_type(Type) -> Default;
default_or_required({_Name, {enum, {list, Type, Default}, _Enumeration}}) when ?is_primitive_type(Type) -> Default;
default_or_required({_Name, {list, {Module, Type}, Default}}) when is_atom(Module), is_atom(Type) -> Default;
default_or_required({_Name, {list, Type, Default}}) when is_atom(Type) -> Default;
default_or_required({Name, {list, {Module, Type}}}) when is_atom(Module), is_atom(Type) -> {required, Name};
default_or_required({Name, {list, Type}}) when is_atom(Type) -> {required, Name};
default_or_required({_Name, {dict, {Module, Type}, {_Key, []}}}) when is_atom(Module), is_atom(Type) ->
    gb_trees:empty();
default_or_required({_Name, {dict, Type, {_Key, []}}}) when is_atom(Type) -> gb_trees:empty();
default_or_required({Name, {dict, {Module, Type}, _Opts}}) when is_atom(Module), is_atom(Type) -> {required, Name};
default_or_required({Name, {dict, Type}, _Opts}) when is_atom(Type) -> {required, Name};
default_or_required({_Name, {option, {Module, Type}}}) when is_atom(Module), is_atom(Type) -> undefined;
default_or_required({_Name, {option, Type}}) when is_atom(Type) -> undefined;
default_or_required({_Name, {Type, undefined}}) when is_atom(Type) -> undefined;
default_or_required({Name, Type}) when is_atom(Type) -> {required, Name};
default_or_required({_Name, {{Module, Type}, undefined}}) when is_atom(Module), is_atom(Type) -> undefined;
default_or_required({Name, {Module, Type}}) when is_atom(Module), is_atom(Type) -> {required, Name};
default_or_required(_Field) -> undefined.

generate_from_json_field(RecordName, {Name, Qualified = {enum, Type, _Enumeration}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Qualified])};
generate_from_json_field(RecordName, {Name, Qualified = {either, _Types}}) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Qualified])};
generate_from_json_field(RecordName, {Name, {enum, {Type, _Default}, Enumeration}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, {enum, Type, Enumeration}])};
generate_from_json_field(RecordName, {Name, Type}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Type])};
generate_from_json_field(RecordName, {Name, Qualified = {enum, {list, Type}, _Enumeration}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Qualified])};
generate_from_json_field(RecordName, {Name, Qualified = {list, Type}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Qualified])};
generate_from_json_field(RecordName, {Name, Qualified = {option, Type}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Qualified])};
generate_from_json_field(RecordName, {Name, {option, Type, _Default}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, {option, Type}])};
generate_from_json_field(RecordName, {Name, {Type, _Default}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Type])};
generate_from_json_field(RecordName, {Name, {list, Type, _Default}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, {list, Type}])};
generate_from_json_field(RecordName, {Name, {enum, {list, Type, _Default}, Enumeration}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, {enum, {list, Type}, Enumeration}])};
generate_from_json_field(RecordName, {Name, {list, {Module, Type}, _Default}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, {list, {Module, Type}}])};
generate_from_json_field(RecordName, {Name, {list, Type, _Default}}) when is_atom(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, {list, {?MODULE, ~p}})}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Type])};
generate_from_json_field(RecordName, {Name, {list, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    generate_from_json_field(RecordName, {Name, {list, {Module, Type}, {required, Name}}});
generate_from_json_field(RecordName, {Name, {list, Type}}) when is_atom(Type) ->
    generate_from_json_field(RecordName, {Name, {list, Type, {required, Name}}});

generate_from_json_field(RecordName, {Name, Qualified = {dict, {Module, Type}, _Opts}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Qualified])};
generate_from_json_field(RecordName, {Name, {dict, Type, Opts}}) when is_atom(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, {dict, {?MODULE, ~p}, ~p})}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Type, Opts])};

generate_from_json_field(RecordName, {Name, Qualified = {option, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Qualified])};
generate_from_json_field(RecordName, {Name, {option, Type}}) when is_atom(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, {option, {?MODULE, ~p}})}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Type])};
generate_from_json_field(RecordName, {Name, {Type, undefined}}) when is_atom(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, {?MODULE, ~p})}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Type])};
generate_from_json_field(RecordName, {Name, Type}) when is_atom(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, {?MODULE, ~p})}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Type])};
generate_from_json_field(RecordName, {Name, {Qualified = {Module, Type}, undefined}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Qualified])};
generate_from_json_field(RecordName, {Name, Qualified = {Module, Type}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("(~p, Value, T) -> T#~p{~p = xl_json_bindc:cast({json, ?JSON_API}, Value, ~p)}", [?JSON_API:field_name_presentation(Name), RecordName, Name, Qualified])};
generate_from_json_field(RecordName, Field) -> {error, {gen_from_json, dont_understand, RecordName, Field}}.

% runtime functions

check_required(Tuple) -> lists:foreach(fun(R = {required, _}) -> error(R); (_) -> ok end, tuple_to_list(Tuple)).

check_enumeration(Value, {seq, F, T}) -> check_enumeration(Value, lists:seq(F, T));
check_enumeration(Value, Enumeration) when is_list(Enumeration) ->
    case lists:member(Value, Enumeration) of
        true -> Value;
        false -> error({illegal_enum_value, Value})
    end.

check_list(string, List) -> check_list_content(fun is_binary/1, List);
check_list(binary, List) -> check_list_content(fun is_binary/1, List);
check_list(integer, List) -> check_list_content(fun is_integer/1, List);
check_list(float, List) -> check_list_content(fun is_float/1, List);
check_list(boolean, List) -> check_list_content(fun is_boolean/1, List);
check_list(atom, List) -> check_list_content(fun is_atom/1, List);
check_list(_, List) -> List.

check_list_content(F, List) when is_list(List) ->
    case lists:all(F, List) of
        true -> List;
        false -> error({illegal_list_value, List})
    end;
check_list_content(_F, List) -> error({illegal_list_value, List}).


is_type(V, integer) when is_integer(V) -> true;
is_type(V, float) when is_float(V) -> true;
is_type(V, atom) when is_binary(V) -> true;
is_type(_V, _Type) -> false.

-spec(cast(module(), any(), term()) -> any()).
cast(_Source, null, _Type) -> undefined;

cast(Source, V, EitherType = {either, Types}) ->
    case xl_lists:find(fun(Type) -> is_type(V, Type) end, Types) of
        {ok, Type} -> cast(Source, V, Type);
        undefined -> error({cannot_cast, V, EitherType})
    end;

cast(_Source, V, {enum, atom, Enumeration}) -> check_enumeration(xl_convert:to(atom, V), Enumeration);
cast(_Source, V, {enum, {list, atom}, Enumeration}) when is_list(V) ->
    [check_enumeration(xl_convert:to(atom, X), Enumeration) || X <- V];
cast(_Source, V, atom) -> xl_convert:to(atom, V);
cast(_Source, V, {list, atom}) -> lists:map(fun(X) -> xl_convert:to(atom, X) end, V);
cast(_Source, V, {option, atom}) -> {ok, xl_convert:to(atom, V)};

cast(_Source, V, {enum, string, Enumeration}) -> check_enumeration(V, Enumeration);
cast(_Source, V, {enum, {list, string}, Enumeration}) when is_list(V) ->
    [check_enumeration(X, Enumeration) || X <- V];
cast(_Source, V, string) when is_binary(V) -> V;
cast(_Source, V, string) when is_integer(V) -> xl_convert:to(binary, V);
cast(_Source, V, {list, string}) -> check_list(string, V);
cast(_Source, V, {option, string}) when is_binary(V) -> {ok, V};
cast(_Source, V, {option, string}) when is_integer(V) -> {ok, xl_convert:to(binary, V)};

cast(_Source, V, {enum, binary, Enumeration}) -> check_enumeration(V, Enumeration);
cast(_Source, V, {enum, {list, binary}, Enumeration}) when is_list(V) ->
    lists:map(fun(X) -> check_enumeration(X, Enumeration) end, V);
cast(_Source, V, binary) when is_binary(V) -> V;
cast(_Source, V, {list, binary}) -> check_list(binary, V);
cast(_Source, V, {option, binary}) when is_binary(V) -> {ok, V};

cast(_Source, V, {enum, integer, Enumeration}) -> check_enumeration(V, Enumeration);
cast(_Source, V, {enum, {list, integer}, Enumeration}) when is_list(V) ->
    [check_enumeration(X, Enumeration) || X <- V];
cast(_Source, V, integer) when is_integer(V) -> V;
cast(_Source, V, integer) when is_binary(V) -> xl_convert:to(integer, V);
cast(_Source, V, {list, integer}) -> V;
cast(_Source, V, {option, integer}) when is_integer(V) -> {ok, V};

cast(_Source, V, {enum, float, Enumeration}) -> check_enumeration(xl_convert:to(float, V), Enumeration);
cast(_Source, V, {enum, {list, float}, Enumeration}) when is_list(V) ->
    [check_enumeration(xl_convert:to(float, X), Enumeration) || X <- V];
cast(_Source, V, float) when is_float(V) -> V;
cast(_Source, V, float) when is_integer(V); is_binary(V) -> xl_convert:to(float, V);
cast(_Source, V, {list, float}) -> [xl_convert:to(float, X) || X <- V];
cast(_Source, V, {option, float}) when is_float(V) -> {ok, V};
cast(_Source, V, {option, float}) when is_integer(V); is_binary(V) -> {ok, xl_convert:to(float, V)};

cast(_Source, true, boolean) -> true;
cast(_Source, false, boolean) -> false;
cast(_Source, <<"true">>, boolean) -> true;
cast(_Source, <<"false">>, boolean) -> false;
cast(_Source, V, {list, boolean}) -> check_list(boolean, V);
cast(_Source, V, {option, boolean}) when V == true; V == false -> {ok, V};

cast({json, JsonApi}, V, any) -> JsonApi:to_abstract(V);
cast({json, JsonApi}, V, {list, any}) when is_list(V) -> [JsonApi:to_abstract(X) || X <- V];
cast({json, _JsonApi}, V, {list, any}) -> error({illegal_list_value, V});
cast({json, JsonApi}, V, {option, any}) -> {ok, JsonApi:to_abstract(V)};

cast(Source, V, {dict, {Module, Record}, {Key, _}}) when is_list(V) -> cast(Source, V, {dict, {Module, Record}, Key});
cast({json, JsonApi}, V, {dict, {Module, Record}, Key}) when is_list(V) ->
    lists:foldl(fun(O, Tree) ->
        Object = Module:from_json_(O, Record),
        case JsonApi:get_value(Key, O) of
            {ok, KeyValue} -> gb_trees:insert(KeyValue, Object, Tree);
            _ -> error({cannot_cast_no_key_field, O, {Module, Record}})
        end

    end, gb_trees:empty(), V);
cast({json, _JsonApi}, V, {list, {Module, Record}}) when is_list(V) -> [Module:from_json_(O, Record) || O <- V];
cast({json, _JsonApi}, V, {list, {_Module, _Record}}) -> error({illegal_list_value, V});
cast({json, _JsonApi}, V, {option, {Module, Record}}) -> {ok, Module:from_json_(V, Record)};
cast({json, _JsonApi}, V, {Module, Record}) -> Module:from_json_(V, Record);

cast(_Source, Value, Type) -> error({cannot_cast, Value, Type}).

-spec(cast(module(), option_m:monad(any()), term(), any()) -> any()).
cast(_Source, {ok, null}, _Type, _Default) -> undefined;

cast(Source, {ok, V}, EitherType = {either, Types}, Default) ->
    case xl_lists:find(fun(Type) -> is_type(V, Type) end, Types) of
        {ok, Type} -> cast(Source, {ok, V}, Type, Default);
        undefined -> error({cannot_cast, V, EitherType})
    end;

cast(_Source, {ok, V}, {enum, atom, Enumeration}, _Default) -> check_enumeration(xl_convert:to(atom, V), Enumeration);
cast(_Source, {ok, V}, {enum, {list, atom}, Enumeration}, _Default) when is_list(V) ->
    [check_enumeration(xl_convert:to(atom, X), Enumeration) || X <- V];
cast(_Source, {ok, V}, atom, _Default) -> xl_convert:to(atom, V);
cast(_Source, {ok, V}, {list, atom}, _Default) -> lists:map(fun(X) -> xl_convert:to(atom, X) end, V);
cast(_Source, {ok, V}, {option, atom}, _Default) -> {ok, xl_convert:to(atom, V)};

cast(_Source, {ok, V}, {enum, string, Enumeration}, _Default) -> check_enumeration(V, Enumeration);
cast(_Source, {ok, V}, {enum, {list, string}, Enumeration}, _Default) when is_list(V) ->
    [check_enumeration(X, Enumeration) || X <- V];
cast(_Source, {ok, V}, string, _Default) when is_binary(V) -> V;
cast(_Source, {ok, V}, string, _Default) when is_integer(V) -> xl_convert:to(binary, V);
cast(_Source, {ok, V}, {list, string}, _Default) when is_list(V) -> check_list(string, V);
cast(_Source, {ok, V}, {option, string}, _Default) when is_binary(V) -> {ok, V};
cast(_Source, {ok, V}, {option, string}, _Default) when is_integer(V) -> {ok, xl_convert:to(binary, V)};

cast(_Source, {ok, V}, {enum, binary, Enumeration}, _Default) -> check_enumeration(V, Enumeration);
cast(_Source, {ok, V}, {enum, {list, binary}, Enumeration}, _Default) when is_list(V) ->
    lists:map(fun(X) -> check_enumeration(X, Enumeration) end, V);
cast(_Source, {ok, V}, binary, _Default) when is_binary(V) -> V;
cast(_Source, {ok, V}, {list, binary}, _Default) when is_list(V) -> check_list(binary, V);
cast(_Source, {ok, V}, {option, binary}, _Default) when is_binary(V) -> {ok, V};

cast(_Source, {ok, V}, {enum, integer, Enumeration}, _Default) -> check_enumeration(V, Enumeration);
cast(_Source, {ok, V}, {enum, {list, integer}, Enumeration}, _Default) when is_list(V) ->
    [check_enumeration(X, Enumeration) || X <- V];
cast(_Source, {ok, V}, integer, _Default) when is_integer(V) -> V;
cast(_Source, {ok, V}, integer, _Default) when is_binary(V) -> xl_convert:to(integer, V);
cast(_Source, {ok, V}, {list, integer}, _Default) when is_list(V) -> V;
cast(_Source, {ok, V}, {option, integer}, _Default) when is_integer(V) -> {ok, V};

cast(_Source, {ok, V}, {enum, float, Enumeration}, _Default) -> check_enumeration(xl_convert:to(float, V), Enumeration);
cast(_Source, {ok, V}, {enum, {list, float}, Enumeration}, _Default) when is_list(V) ->
    [check_enumeration(xl_convert:to(float, X), Enumeration) || X <- V];
cast(_Source, {ok, V}, float, _Default) when is_float(V) -> V;
cast(_Source, {ok, V}, float, _Default) when is_integer(V); is_binary(V) -> xl_convert:to(float, V);
cast(_Source, {ok, V}, {list, float}, _Default) when is_list(V) -> [xl_convert:to(float, X) || X <- V];
cast(_Source, {ok, V}, {option, float}, _Default) when is_float(V) -> {ok, V};
cast(_Source, {ok, V}, {option, float}, _Default) when is_integer(V); is_binary(V) -> {ok, xl_convert:to(float, V)};

cast(_Source, {ok, true}, boolean, _Default) -> true;
cast(_Source, {ok, false}, boolean, _Default) -> false;
cast(_Source, {ok, <<"true">>}, boolean, _Default) -> true;
cast(_Source, {ok, <<"false">>}, boolean, _Default) -> false;
cast(_Source, {ok, V}, {list, boolean}, _Default) when is_list(V) -> V;
cast(_Source, {ok, V}, {option, boolean}, _Default) when V == true; V == false -> {ok, V};

cast(proplist, {ok, V}, any, _Default) -> V;
cast(proplist, {ok, V}, {list, any}, _Default) when is_list(V) -> V;
cast(proplist, {ok, V}, {option, any}, _Default) -> {ok, V};

%% cast(proplist, {ok, V}, {list, {Module, Record}}, _Default) when is_list(V) -> [Module:from_proplist_(O, Record) || O <- V];
cast(proplist, {ok, V}, {option, {Module, Record}}, _Default) -> {ok, Module:from_proplist_(V, Record)};
cast(proplist, {ok, V}, {Module, Record}, _Default) -> Module:from_proplist_(V, Record);

cast(_Source, undefined, _Type, E = {required, _}) -> error(E);

cast(_Source, undefined, {option, _}, undefined) -> undefined;
cast(_Source, undefined, {option, _}, Default) -> {ok, Default};
cast(_Source, undefined, _Type, Default) -> Default;

cast(_Source, Value, Type, _Default) -> error({cannot_cast, Value, Type}).

generate_from_proplist(Records, Out) ->
    do([error_m ||
        Functions <- xl_lists:emap(fun({RecordName, Fields}) ->
            do([error_m ||
                Generated <- xl_lists:emap(fun(Field) ->
                    generate_from_proplist_field(Field) end, serializable_fields(Fields)),
                return(
                    xl_string:format(
                        "from_proplist_(J, ~p) ->\n\t#~p{\n~s\n\t}",
                        [RecordName, RecordName, xl_string:join([xl_string:format("\t\t~s", [F]) || F <- Generated], ",\n")]
                    )
                )
            ])
        end, Records),
        file:write(Out, xl_string:join(Functions, ";\n") ++ ".\n\n")
    ]).

generate_from_proplist_field({Name, Qualified = {enum, Type, _Enumeration}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, Qualified, {required, Name}])};
generate_from_proplist_field({Name, Qualified = {either, _Types}}) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, Qualified, {required, Name}])};
generate_from_proplist_field({Name, {enum, {Type, Default}, Enumeration}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, {enum, Type, Enumeration}, Default])};
generate_from_proplist_field({Name, Type}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, Type, {required, Name}])};
generate_from_proplist_field({Name, Qualified = {enum, {list, Type}, _Enumeration}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, Qualified, {required, Name}])};
generate_from_proplist_field({Name, Qualified = {list, Type}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, Qualified, {required, Name}])};
generate_from_proplist_field({Name, Qualified = {option, Type}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, Qualified, undefined])};
generate_from_proplist_field({Name, {option, Type, Default}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, {option, Type}, Default])};
generate_from_proplist_field({Name, {Type, Default}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, Type, Default])};
generate_from_proplist_field({Name, {list, Type, Default}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, {list, Type}, Default])};
generate_from_proplist_field({Name, {enum, {list, Type, Default}, Enumeration}}) when ?is_primitive_type(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, {enum, {list, Type}, Enumeration}, Default])};
generate_from_proplist_field({Name, {list, {Module, Type}, Default}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, {list, {Module, Type}}, Default])};
generate_from_proplist_field({Name, {list, Type, Default}}) when is_atom(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), {list, {?MODULE, ~p}}, ~p)", [Name, Name, Type, Default])};
generate_from_proplist_field({Name, {list, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    generate_from_proplist_field({Name, {list, {Module, Type}, {required, Name}}});
generate_from_proplist_field({Name, {list, Type}}) when is_atom(Type) ->
    generate_from_proplist_field({Name, {list, Type, {required, Name}}});

generate_from_proplist_field({Name, Qualified = {dict, {Module, Type}, {_, Default}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), ~p, ~p)", [Name, Name, Qualified, Default])};
generate_from_proplist_field({Name, Qualified = {dict, Type, {_, Default}}}) when is_atom(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, xl_lists:kvfind(~p, J), {list, {?MODULE, ~p}}, ~p)", [Name, Name, Qualified, Default])};
generate_from_proplist_field({Name, {dict, {Module, Type}, Key}}) when is_atom(Module), is_atom(Type) ->
    generate_from_proplist_field({Name, {dict, {Module, Type}, {Key, {required, Name}}}});
generate_from_proplist_field({Name, {dict, Type, Key}}) when is_atom(Type) ->
    generate_from_proplist_field({Name, {dict, Type, {Key, {required, Name}}}});

generate_from_proplist_field({Name, Qualified = {option, {Module, Type}}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, {ok, J}, ~p, ~p)", [Name, Qualified, undefined])};
generate_from_proplist_field({Name, {option, Type}}) when is_atom(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, {ok, J}, {option, {?MODULE, ~p}}, ~p)", [Name, Type, undefined])};
generate_from_proplist_field({Name, {Type, undefined}}) when is_atom(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, {ok, J}, {?MODULE, ~p}, ~p)", [Name, Type, undefined])};
generate_from_proplist_field({Name, Type}) when is_atom(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, {ok, J}, {?MODULE, ~p}, ~p)", [Name, Type, {required, Name}])};
generate_from_proplist_field({Name, {Qualified = {Module, Type}, undefined}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, {ok, J}, ~p, ~p)", [Name, Qualified, undefined])};
generate_from_proplist_field({Name, Qualified = {Module, Type}}) when is_atom(Module), is_atom(Type) ->
    {ok, xl_string:format("~p = xl_json_bindc:cast(proplist, {ok, J}, ~p, ~p)", [Name, Qualified, {required, Name}])};
generate_from_proplist_field(Field) -> {error, {gen_from_proplist, dont_understand, Field}}.

format(undefined) -> null;
format(true) -> true;
format(false) -> false;
format(X) when is_atom(X) -> format(atom_to_binary(X, utf8));
format({ok, X}) -> format(X);
format(X) when is_binary(X) ->
    [<<"\"">>, quote(X, <<>>), <<"\"">>];
format(X = [H | _]) when is_tuple(H) andalso size(H) == 2 ->
    [<<"{">>, xl_lists:disperse([[<<"\"">>, K, <<"\":">>, format(V)] || {K, V} <- X], <<",">>), <<"}">>];
format(X) when is_list(X) -> [<<"[">>, xl_lists:disperse([format(E) || E <- X], <<",">>), <<"]">>];
format(X) -> X.

quote(<<>>, Acc) -> Acc;
quote(<<$", Rest/binary>>, Acc) -> quote(Rest, <<Acc/binary, $\\, $">>);
quote(<<$\n, Rest/binary>>, Acc) -> quote(Rest, <<Acc/binary, $\\, $n>>);
quote(<<$\r, Rest/binary>>, Acc) -> quote(Rest, <<Acc/binary, $\\, $r>>);
quote(<<$\t, Rest/binary>>, Acc) -> quote(Rest, <<Acc/binary, $\\, $t>>);
quote(<<$\\, Rest/binary>>, Acc) -> quote(Rest, <<Acc/binary, $\\, $\\>>);
quote(<<X, Rest/binary>>, Acc) -> quote(Rest, <<Acc/binary, X>>).
