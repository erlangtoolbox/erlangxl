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
-module(xl_io).

-export([lines/1, parse_lines/1, posix_error/2, apply_io/3, is_posix_error/1]).
-type posix_error() :: {error, {atom(), atom() | string(), any()}}.
-export_types([posix_error/0]).

lines(IoDevice) -> xl_stream:map(fun({L, _}) -> L end, parse_lines(IoDevice)).

parse_lines(IoDevice) ->
    xl_stream:stream(IoDevice, fun(_) ->
        {ok, Pos} = file:position(IoDevice, {cur, 0}),
        case io:get_line(IoDevice, '') of
            eof -> empty;
            L -> {{list_to_binary(L), Pos}, IoDevice}
        end
    end).

-spec posix_error/2 :: ({error, atom()}, any()) -> posix_error().
posix_error(E = {error, Code}, Target) ->
    case is_posix_error(E) of
        true -> {error, {Code, erl_posix_msg:message(Code), Target}};
        _ -> {error, {Code, Code, Target}}
    end.

-spec is_posix_error/1 :: ({error, atom()}) -> boolean().
is_posix_error({error, Code}) -> erl_posix_msg:message(Code) /= "unknown POSIX error".

-spec apply_io/3 :: (module(), atom(), [term()]) -> any() | posix_error().
apply_io(Module, Func, Args) ->
    case apply(Module, Func, Args) of
        E = {error, _} -> posix_error(E, Args);
        X -> X
    end.

