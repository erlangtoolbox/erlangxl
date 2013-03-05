%% Copyright
-module(xl_state_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

keys_test() ->
    application:start(sasl),
    erlang:display(application:start(xl_stdlib)),
    xl_state:new(test),
    xl_state:set(test, a, a),
    xl_state:set(test, b, b),
    ?assertEqual([b, a], xl_state:keys(test)).
