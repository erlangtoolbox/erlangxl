-module(strikead_url_tests).

-include_lib("eunit/include/eunit.hrl").

to_query_test() ->
	?assertEqual("landing=http%3a%2f%2fstrikead.com&a=1",
		strikead_url:to_query([{landing, "http://strikead.com"}, {a, 1}])).
