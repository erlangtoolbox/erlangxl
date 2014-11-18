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
%%% Created : 02. Feb 2014 9:32 PM
%%%-------------------------------------------------------------------
-module(xl_code_appdesc).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, do}).

%% API
-export([load/1, update/2, appfile/1]).

-spec(load(file:name()) -> error_m:monad(application:application_spec())).
load(Dir) ->
    case appfile(Dir) of
        {ok, File} ->
            case xl_file:read_terms(File) of
                {ok, [App = {application, _, Params}]} when is_list(Params) ->
                    {ok, App};
                E = {error, _} -> E;
                _ -> {error, "wrong format of app file"}
            end;
        _ -> {error, "failed to locate single app file in " ++ Dir}
    end.

-spec(appfile(file:name()) -> option_m:monad(file:name())).
appfile(Dir) ->
    case xl_file:wildcard(Dir ++ "/*.app") of
        [File] -> {ok, File};
        _ -> undefined
    end.


-spec(update(application:application_spec(), [application:application_opt()]) -> application:application_spec()).
update({application, App, Params}, Updates) ->
    {application, App, lists:foldl(fun(P, Ps) -> xl_lists:keyreplace_or_add(1, Ps, P) end, Params, Updates)}.
