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
-module(xl_uid).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

%% API
-export([next/0, next_hex/0, start/0]).

next() -> {ok, [P]} = xl_state:get(?MODULE, prefix),
    P + xl_calendar:now_micros().

next_hex() ->
    integer_to_list(next(), 16).

start() ->
    xl_state:new(?MODULE, [{read_concurrency, true}]),
    case inet:getifaddrs() of
        {ok, Ifs} ->
            case xl_lists:find(fun({_, Opts}) ->
                case xl_lists:kvfind(hwaddr, Opts) of
                    {ok, HWAddr} -> lists:any(fun(X) -> X /= 0 end, HWAddr);
                    undefined -> false
                end
            end, Ifs) of
                {ok, {_, Opts}} ->
                    {ok, HWAddr} = xl_lists:kvfind(hwaddr, Opts),
                    <<P:48>> = list_to_binary(HWAddr),
                    xl_state:set(?MODULE, prefix, P bsl 64);
                undefined ->
                    xl_state:set(?MODULE, prefix, 0)
            end;
        _ ->
            xl_state:set(?MODULE, prefix, 0)
    end.