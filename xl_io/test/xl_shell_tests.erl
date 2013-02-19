%% Copyright
-module(xl_shell_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

getenv_test() ->
    Env = xl_shell:getenv(),
    erlang:display(Env),
    lists:foreach(fun({K, V}) ->
        ?assert(is_atom(K)),
        ?assert(is_list(V))
    end, Env).
