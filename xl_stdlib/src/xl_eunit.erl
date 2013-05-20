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
-module(xl_eunit).

-include_lib("eunit/include/eunit.hrl").

-export([resource/2, explode/3, performance/3, format/2, format/1, profile/3, performance/5, profile/5]).

resource(Module, Name) -> within(Module, fun(Path) -> filename:join(Path, Name) end).

explode(Module, Name, Target) ->
    within(Module, fun(Path) ->
        xl_file:copy(filename:join(Path, Name), Target)
    end).


within(Module, Fun) ->
    case code:which(Module) of
        non_existing -> {error, {non_existing, Module}};
        X -> Fun(filename:dirname(X))
    end.

performance(Name, Fun, Count) -> performance(Name, Fun, Count, 1, 10000000).

performance(Name, Fun, Count, Threads, Timeout) ->
    case timer:tc(fun perform_test/4, [Fun, Count, Threads, Timeout]) of
        {_, timeout} -> error(timeout);
        {Time, _} ->
            Xps = Count / Time * 1000000,
            xl_eunit:format("PERFORMANCE ~s: ~.1f op/s, time: ~p ms, threads: ~p~n", [Name, Count * 1000000 / Time, Time / 1000, Threads]),
            Xps
    end.

perform_test(Fun, Count, 1, _Timeout) -> xl_lists:times(Fun, Count);
perform_test(Fun, Count, Threads, Timeout) ->
    Self = self(),
    xl_lists:times(fun() ->
        spawn(fun() ->
            xl_lists:times(Fun, Count),
            Self ! done
        end)
    end, Threads),
    wait(Threads, Timeout).

wait(0, _Timeout) -> ok;
wait(Threads, Timeout) ->
    receive
        done -> wait(Threads - 1, Timeout)
    after Timeout -> timeout
    end.

-spec(format(string(), [term()]) -> ok).
format(Format, Args) -> io:format(user, Format, Args).

-spec(format(string()) -> ok).
format(Format) -> format(Format, []).

profile(Name, Fun, Count) -> profile(Name, Fun, Count, 1, 1000000).

profile(Name, Fun, Count, Threads, Timeout) ->
    fprof:trace([start, {procs, all}]),
    performance(Name, Fun, Count, Threads, Timeout),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse(dest, xl_string:format("/tmp/~s.analysis", [Name])).
