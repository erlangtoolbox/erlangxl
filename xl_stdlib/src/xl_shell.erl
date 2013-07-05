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
-module(xl_shell).

-export([command/1, command/2, getenv/0, exec/2, exec/1]).

-spec(command(string()) -> error_m:monad(string())).
command(Command) ->
    case eunit_lib:command(Command) of
        {0, Out} -> {ok, Out};
        {_, Out} -> {error, Out}
    end.

-spec(command(string(), file:name()) -> error_m:monad(string())).
command(Command, Dir) ->
    case eunit_lib:command(Command, Dir) of
        {0, Out} -> {ok, Out};
        {_, Out} -> {error, Out}
    end.

-spec(getenv() -> [{atom(), string()}]).
getenv() ->
    lists:map(fun(V) ->
        {H, [_ | T]} = lists:splitwith(fun(X) -> X /= $= end, V),
        {list_to_atom(H), T}
    end, os:getenv()).

-spec(exec(string()) -> error_m:monad(string())).
exec(Command) ->
    Port = erlang:open_port({spawn, Command}, [exit_status, {line, 1000}]),
    exec_read(Port).

-spec(exec(string(), file:name()) -> error_m:monad(string())).
exec(Command, Dir) ->
    Port = erlang:open_port({spawn, Command}, [{cd, Dir}, exit_status, {line, 1000}]),
    exec_read(Port).

exec_read(Port) ->
    receive
        {Port, {data, {eol, Line}}} ->
            io:format("~s~n", [Line]),
            exec_read(Port);
        {Port, {data, {noeol, Line}}} ->
            io:format("~s", [Line]),
            exec_read(Port);
        {Port, {exit_status, 0}} -> ok;
        {Port, {exit_status, X}} -> {error, X}
    end.

