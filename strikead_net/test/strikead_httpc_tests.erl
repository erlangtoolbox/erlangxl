-module(strikead_httpc_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("strikead_eunit/include/strikead_eunit.hrl").
-include("strikead_httpc.hrl").

call_test() ->
	inets:start(),
	application:set_env(strikead_net_test, test, [{client, []}, {request, []}]),
	?assertOk(strikead_httpc:start_link(strikead_net_test, test)),
	try
		?assertEqual({ok, #http_response{code=200, reason="OK"}},
			strikead_httpc:call(test, "http://strikead.com"))
	after
		strikead_httpc:stop(test),
		inets:stop()
	end.


post_test() ->
	inets:start(),
	application:set_env(strikead_net_test, test, [{client, []}, {request, []}]),
	?assertOk(strikead_httpc:start_link(strikead_net_test, test)),
	try
		?assertOk(strikead_httpc:post(test, "http://strikead.com", "text/plain", "test"))
	after
		strikead_httpc:stop(test),
		inets:stop()
	end.
