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
-module(xl_tsv).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([parse_line/1, lines/1, parse_file/1, parse_file/2, comment_filter/1]).

parse_line(L) when is_binary(L) -> binary:split(binary:replace(L, [<<"\n">>, <<"\r">>], <<>>), <<"\t">>, [global]);
parse_line(L) when is_list(L) -> xl_lists:split_by(L, $\t).

-spec lines/1 :: (xl_stream:stream()) -> xl_stream:stream().
lines(S) -> xl_stream:map(fun(L) -> parse_line(L) end, S).

-spec parse_file/1 :: (file:name()) -> error_m:monad([[string()]]).
parse_file(Path) -> parse_file(Path, fun(_) -> true end).

-spec parse_file/2 :: (file:name(), fun((binary()) -> boolean())) -> error_m:monad([[string()]]).
parse_file(Path, LineFilter) ->
    xl_file:using(Path, [read], fun(File) ->
        xl_stream:to_list(lines(xl_stream:filter(LineFilter, xl_io:lines(File))))
    end).

-spec comment_filter/1 :: ([char()]) -> fun((binary()) -> boolean()).
comment_filter(Chars) ->
    fun(L) ->
        case binary:replace(L, [<<" ">>, <<"\t">>, <<"\n">>, <<"\r">>], <<"">>, [global]) of
            <<>> -> false;
            <<X, _Rest/binary>> -> not lists:member(X, Chars)
        end
    end.