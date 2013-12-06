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
-module(xl_csv).

-export([parse_line/1, lines/1, parse_file/1]).

parse_line(L) when is_binary(L) -> [list_to_binary(X) || X <- parse_line(binary_to_list(L))];
parse_line("") -> [];
parse_line(L) ->
    case parse(L, "", []) of
        parse_error -> [];
        X -> X
    end.

parse([], Field, Acc) -> lists:reverse([lists:reverse(Field) | Acc]);
parse([$\r | T], Field, Acc) -> parse(T, Field, Acc);
parse([$\n | T], Field, Acc) -> parse(T, Field, Acc);
parse([$" | T], Field, Acc) -> parse_quoted(T, Field, Acc);
parse([$, | T], Field, Acc) -> parse(T, "", [lists:reverse(Field) | Acc]);
parse([H | T], Field, Acc) -> parse(T, [H |Field], Acc);
parse(_, _, _) -> parse_error.

parse_quoted([$", $" | T], Field, Acc) -> parse_quoted(T, [$" | Field], Acc);
parse_quoted([$" | T], Field, Acc) -> parse(T, Field, Acc);
parse_quoted([H | T], Field, Acc) -> parse_quoted(T, [H | Field], Acc);
parse_quoted(_, _, _) -> parse_error.

lines(S) -> xl_stream:map(fun(L) -> parse_line(L) end, S).

-spec(parse_file/1 :: (file:name()) -> error_m:monad([[string()]])).
parse_file(Path) ->
    xl_file:using(Path, [read], fun(File) ->
        xl_stream:to_list(lines(xl_io:lines(File)))
    end).
