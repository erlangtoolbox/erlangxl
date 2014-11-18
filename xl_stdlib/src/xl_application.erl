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
-module(xl_application).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

-export([eget_env/2, get_env/3, is_started/1, start/1]).

-spec(eget_env(atom(), atom()) -> error_m:monad(any())).
eget_env(Application, Env) ->
    case application:get_env(Application, Env) of
        undefined -> {error, {undefined, Application, Env}};
        X -> X
    end.

-spec(get_env(atom(), atom(), term()) -> term()).
get_env(Application, Env, Default) ->
    case application:get_env(Application, Env) of
        {ok, Val} -> Val;
        _ -> Default
    end.

-spec(is_started(atom()) -> boolean()).
is_started(App) -> lists:keymember(App, 1, application:which_applications()).

%% @doc Recursive application start
-spec(start(atom()) -> error_m:monad(ok)).
start(App) ->
    do([error_m ||
        ensure_loaded(App),
        case application:get_key(App, applications) of
            {ok, Deps} -> xl_lists:eforeach(fun(Dep) -> start(Dep) end, Deps);
            _ -> ok
        end,
        application:ensure_started(App)
    ]).

-spec(ensure_loaded(atom()) -> error_m:monad(ok)).
ensure_loaded(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, _}} -> ok;
        E -> E
    end.
