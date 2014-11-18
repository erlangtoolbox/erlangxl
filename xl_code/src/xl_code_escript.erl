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
%%% Created : 02. Feb 2014 10:33 PM
%%%-------------------------------------------------------------------
-module(xl_code_escript).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, do}).

-include_lib("stdlib/include/zip.hrl").

-export([read_file/2, read_file/1, unpack_priv/1, unpack_priv/2]).

-spec(read_file/1 :: (file:name()) -> error_m:monad(binary())).
read_file(File) -> read_file(escript:script_name(), File).

-spec(read_file/2 :: (file:name(), file:name()) -> error_m:monad(binary())).
read_file(EscriptPath, File) ->
    do([error_m ||
        Sections <- escript:extract(EscriptPath, []),
        case xl_lists:find(fun(Section) ->
            element(1, Section) == archive
        end, Sections) of
            undefined -> {ok, undefined};
            {ok, {_, Zip}} ->
                do([error_m ||
                    Handle <- zip:zip_open(Zip, [memory]),
                    try zip:zip_get(File, Handle) of
                        {ok, {_, Bin}} -> {ok, Bin};
                        {error, E} -> {error, {E, [EscriptPath, File]}}
                    after
                        zip:zip_close(Handle)
                    end
                ])
        end
    ]).

unpack_priv(Dir) -> unpack_priv(escript:script_name(), Dir).

unpack_priv(EscriptPath, Dir) ->
    do([error_m ||
        Sections <- escript:extract(EscriptPath, []),
        xl_lists:eforeach(fun
            ({archive, Zip}) ->
                xl_file:mkdirs(Dir),
                zip:extract(Zip, [
                    {cwd, Dir},
                    {file_filter, fun(#zip_file{name = Name}) -> lists:prefix("priv", Name) end}
                ]);
            (_) -> ok
        end, Sections)
    ]).
