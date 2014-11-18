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
-export([parse_line/1, parse_file/1, parse_file/2, comment_filter/1]).

parse_line(L) when is_binary(L) -> binary:split(binary:replace(L, [<<"\n">>, <<"\r">>], <<>>), <<"\t">>, [global]);
parse_line(L) when is_list(L) -> xl_lists:split_by(L, $\t).

-spec(parse_file(file:name()) -> error_m:monad([[string()]])).
parse_file(Path) -> parse_file(Path, fun(_) -> true end).

-spec(parse_file(file:name(), fun((binary()) -> boolean())) -> error_m:monad([[string()]])).
parse_file(Path, LineFilter) ->
    case xl_file:read_lines(Path) of
        {ok, Lines} -> {ok, lists:map(fun(L) -> parse_line(L) end, lists:filter(LineFilter, Lines))};
        E -> E
    end.

-spec(comment_filter([char()]) -> fun((binary()) -> boolean())).
comment_filter(Chars) ->
    fun(L) ->
        case binary:replace(L, [<<" ">>, <<"\t">>, <<"\n">>, <<"\r">>], <<"">>, [global]) of
            <<>> -> false;
            <<X, _Rest/binary>> -> not lists:member(X, Chars)
        end
    end.