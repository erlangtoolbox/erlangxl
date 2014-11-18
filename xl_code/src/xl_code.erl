%% =============================================================================
%%  The MIT License (MIT)
%%
%%  Copyright (c) 2014 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy of
%%  this software and associated documentation files (the "Software"), to deal in
%%  the Software without restriction, including without limitation the rights to
%%  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
%%  the Software, and to permit persons to whom the Software is furnished to do so,
%%  subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in all
%%  copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
%%  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
%%  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
%%  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%%  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%% =============================================================================

%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2014 2:45 AM
%%%-------------------------------------------------------------------
-module(xl_code).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, do}).

%% API
-export([escape_applications/4]).

-spec(escape_applications(atom(), [string()], file:name(), [term()])
            -> error_m:monad([{atom(), Version :: string()}])).
escape_applications(Prefix, AppsWildcards, TargetDir, Excludes) ->
    Apps = xl_file:wildcards(AppsWildcards),
    Modules = lists:map(fun(M) ->
        list_to_atom(filename:basename(M, ".beam"))
    end, xl_file:wildcards([App ++ "/ebin/*.beam" || App <- Apps])),
    AppNames = lists:map(fun(A) ->
        list_to_atom(filename:basename(A, ".app"))
    end, xl_file:wildcards([App ++ "/ebin/*.app" || App <- Apps])),
    xl_lists:emap(fun(App) ->
        escape(TargetDir, App, Prefix, Modules ++ AppNames, Excludes)
    end, Apps).


escape(TargetDir, AppDir, Prefix, Atoms, Excludes) ->
    do([error_m ||
        A = {application, Name, AppParams} <- xl_code_appdesc:load(AppDir ++ "/ebin"),
        NewAppName <- return(translate(Name, Prefix, Atoms, Excludes)),
        TargetAppDir <- case xl_lists:kvfind(vsn, AppParams) of
            {ok, Vsn} -> return(xl_string:join([TargetDir, "/", NewAppName, '-', Vsn]));
            undefined -> return(xl_string:join([TargetDir, "/", NewAppName]))
        end,
        xl_lists:eforeach(fun(Mod) ->
            case beam_lib:chunks(Mod, [abstract_code]) of
                {error, beam_lib, Error} -> {error, {Mod, Error}};
                {ok, {_, [{abstract_code, missing_chunk}]}} -> {error, {Mod, no_abstract_code}};
                {ok, {_, [{abstract_code, no_abstract_code}]}} -> {error, {Mod, no_abstract_code}};
                {ok, {_, [{abstract_code, {_, Forms}}]}} ->
                    case compile:forms(escape_forms(Prefix, Forms, Atoms, Excludes), [report, debug_info]) of
                        {ok, Module, Binary} ->
                            xl_file:write(xl_string:join([TargetAppDir, "/ebin/", Module, ".beam"]), Binary);
                        error -> error
                    end
            end
        end, xl_file:wildcard(xl_string:join([AppDir, "/ebin/*.beam"]))),
        xl_file:write_term(
            xl_string:join([TargetAppDir, "/ebin/", NewAppName, ".app"]),
            translate(A, Prefix, Atoms, Excludes)
        ),
        xl_file:copy_if_exists(AppDir ++ "/priv", TargetAppDir),
        return({NewAppName, xl_lists:kvfind(vsn, AppParams, undefined)})
    ]).

translate(List, Prefix, Atoms, Excludes) when is_list(List) ->
    [translate(E, Prefix, Atoms, Excludes) || E <- List];
translate(Tuple, Prefix, Atoms, Excludes) when is_tuple(Tuple) ->
    list_to_tuple([translate(E, Prefix, Atoms, Excludes) || E <- tuple_to_list(Tuple)]);
translate(A, Prefix, Atoms, Excludes) when is_atom(A) ->
    case lists:member(A, Atoms) andalso not lists:member(A, Excludes) of
        true -> xl_string:join_atom([Prefix, A]);
        false -> A
    end;
translate(X, _, _, _) -> X.

escape_forms(Escape, Forms, Atoms, Excludes) -> [escape_form(Escape, Form, Atoms, Excludes) || Form <- Forms].

escape_form(Escape, T = {atom, Line, Name}, Atoms, Excludes) ->
    case lists:member(Name, Atoms) andalso not is_exclude(T, Name, Excludes) of
        true -> {atom, Line, xl_string:join_atom([Escape, Name])};
        false -> T
    end;
escape_form(Escape, T = {attribute, Line, module, {Name, Params}}, _Atoms, Excludes) ->
    case not is_exclude(T, Name, Excludes) of
        true -> {attribute, Line, module, {xl_string:join_atom([Escape, Name]), Params}};
        false -> T
    end;
escape_form(Escape, T = {attribute, Line, module, Name}, _Atoms, Excludes) ->
    case not is_exclude(T, Name, Excludes) of
        true -> {attribute, Line, module, xl_string:join_atom([Escape, Name])};
        false -> T
    end;
escape_form(Escape, X, Atoms, Excludes) when is_tuple(X) ->
    list_to_tuple([escape_form(Escape, E, Atoms, Excludes) || E <- tuple_to_list(X)]);
escape_form(Escape, X, Atoms, Excludes) when is_list(X) -> [escape_form(Escape, E, Atoms, Excludes) || E <- X];
escape_form(_Escape, X, _Atoms, _Excludes) -> X.

is_exclude(Tuple, Name, Excludes) -> lists:member(Tuple, Excludes) orelse lists:member(Name, Excludes).