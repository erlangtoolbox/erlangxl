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
-module(xl_escript).

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
