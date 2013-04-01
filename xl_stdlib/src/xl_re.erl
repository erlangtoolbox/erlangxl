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
-module(xl_re).
-author("andrei.zavada@strikead.com").

-export([start/0, compile/2,
        run/2, split/2,
        run/3, split/3]).



-spec start() -> ok.
start() ->
    xl_state:new(?MODULE, [{read_concurrency, true}]),
    ok.


-type option() :: [term()].  %% refer to man 3 re
-type mp() :: term().
-type hash() :: integer().

-spec run(Subject :: iodata() | io:charlist(),
        RE :: mp() | iodata() | integer(),
        Options :: [option()]) ->
            {match, Captured :: [term()]} | nomatch |
            {error, not_compiled}.
run(Subject, RE) -> run(Subject, RE, []).
run(Subject, RE, Options) -> apply_fun(Subject, RE, Options, fun re:run/3).

-spec split(Subject :: iodata() | io:charlist(),
        RE :: mp() | iodata() | integer(),
        Options :: [option()]) ->
            {match, Captured :: [term()]} | nomatch |
            {error, not_compiled}.
split(Subject, RE) ->
    split(Subject, RE, []).
split(Subject, RE, Options) ->
    apply_fun(Subject, RE, Options, fun re:split/3).

-spec compile(RE :: iodata(),
        Options :: [option()]) ->
            {ok, Hash :: hash()} |
            {error, {ErrString :: string(), Position :: non_neg_integer()}}.
compile(RE, Options) ->
    Key = erlang:phash2({RE, Options}),
    case xl_state:get(?MODULE, Key) of
        {ok, _} ->
            {ok, Key};
        undefined ->
            case re:compile(RE, compile_safe(Options)) of
                {ok, MP} ->
                    _ = xl_state:set(?MODULE, Key, MP),
                    {ok, Key};
                ErrorReason ->
                    ErrorReason
            end
    end.


%% supporting functions

apply_fun(Subject, Key, Options, Fun) when is_integer(Key) ->
    case xl_state:get(?MODULE, Key) of
        {ok, MP} ->
            Fun(Subject, MP, mp_safe(Options));
        undefined ->
            {error, not_compiled}
    end;
apply_fun(Subject, RE, Options, Fun) ->
    Key = erlang:phash2({RE, Options}),
    case xl_state:get(?MODULE, Key) of
        {ok, [MP]} ->
            Fun(Subject, MP, mp_safe(Options));
        [] ->
            case re:compile(RE, compile_safe(Options)) of
                {ok, MP} ->
                    xl_state:set(?MODULE, Key, MP),
                    Fun(Subject, MP, mp_safe(Options));
                ErrorReason ->
                    ErrorReason
            end
    end.

compile_safe(Options) ->
    lists:filter(
            fun(unicode) -> true;
                (anchored) -> true;
                (caseless) -> true;
                (dollar_endonly) -> true;
                (dotall) -> true;
                (extended) -> true;
                (firstline) -> true;
                (multiline) -> true;
                (no_auto_capture) -> true;
                (dupnames) -> true;
                (ungreedy) -> true;
                ({newline, _}) -> true;
                (_) -> false
            end, Options).

mp_safe(Options) ->
    lists:filter(
            fun(anchored) -> true;
                (global) -> true;
                (notbol) -> true;
                (noteol) -> true;
                (notempty) -> true;
                ({offset, _}) -> true;
                ({newline, _}) -> true;
                ({capture, _}) -> true;
                ({capture, _, _}) -> true;
                (_) -> false
            end, Options).
