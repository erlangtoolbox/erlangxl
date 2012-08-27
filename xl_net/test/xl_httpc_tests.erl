-module(xl_httpc_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_eunit/include/xl_eunit.hrl").
-include("xl_httpc.hrl").

call_test() ->
    inets:start(),
    application:set_env(xl_net_test, test, [{client, []}, {request, []}]),
    ?assertOk(xl_httpc:start_link(xl_net_test, test)),
    try
        ?assertEqual({ok, #http_resp{code = 200, reason = "OK"}},
            xl_httpc:call(test, "http://strikead.com"))
    after
        xl_httpc:stop(test),
        inets:stop()
    end.


post_test() ->
    inets:start(),
    application:set_env(xl_net_test, test, [{client, []}, {request, []}]),
    ?assertOk(xl_httpc:start_link(xl_net_test, test)),
    try
        ?assertOk(xl_httpc:post(test, "http://strikead.com", "text/plain", "test"))
    after
        xl_httpc:stop(test),
        inets:stop()
    end.

