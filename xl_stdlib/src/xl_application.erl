%% Copyright
-module(xl_application).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([eget_env/2]).

-spec eget_env/2 :: (atom(), atom()) -> error_m:monad(any()).
eget_env(Application, Env) ->
    option_m:to_error_m(application:get_env(Application, Env), {undefined, {Application, Env}}).
