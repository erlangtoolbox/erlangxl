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
-module(xl_lang).

-export([ifelse/3, record_to_proplist/2, safe_call/2, register/2, unregister/1, find_nif/2, priv_dir/2, priv_path/3, delete_element/2]).

ifelse(true, Then, _) -> result(Then);
ifelse(false, _, Else) -> result(Else).

record_to_proplist(Record, Fields) ->
    xl_lists:imap(fun(Field, I) -> {Field, element(I + 1, Record)} end, Fields).

safe_call(F, R) when is_function(F, 0) ->
    try
        F()
    catch
        _:_ -> result(R)
    end.

result(R) when is_function(R, 0) -> R();
result(R) -> R.

-spec(register(atom(), pid() | port()) -> error_m:monad(ok)).
register(Name, PidOrPort) ->
    try erlang:register(Name, PidOrPort) of
        true -> {ok, PidOrPort}
    catch
        _ : badarg -> {error, {cannot_register, Name, PidOrPort}}
    end.

-spec(unregister(atom()) -> error_m:monad(ok)).
unregister(Name) ->
    try erlang:unregister(Name) of
        true -> ok
    catch
        _ : badarg -> {error, {cannot_unregister, Name}}
    end.

find_nif(App, Module) ->
    filename:join(priv_dir(App, Module), atom_to_list(Module)).

priv_dir(App, Module) ->
    case code:priv_dir(App) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(Module)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path -> Path
    end.

priv_path(App, Module, Path) -> filename:join(priv_dir(App, Module), Path).


-spec(delete_element(pos_integer(), tuple()) -> tuple()).
delete_element(N, Tuple) ->
    L = tuple_to_list(Tuple),
    {H, [_ | T]} = lists:split(N - 1, L),
    list_to_tuple(H ++ T).