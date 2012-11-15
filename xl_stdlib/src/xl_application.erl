%% Copyright
-module(xl_application).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([eget_env/2, get_env/3]).

-spec(eget_env(atom(), atom()) -> error_m:monad(any())).
eget_env(Application, Env) ->
    option_m:to_error_m(application:get_env(Application, Env), {undefined, {Application, Env}}).

-spec(get_env(atom(), atom(), term()) -> term()).
get_env(Application, Env, Default) ->
    case application:get_env(Application, Env) of
        {ok, Val} -> Val;
        _ -> Default
    end.
