-module(xl_url_tests).

-include_lib("eunit/include/eunit.hrl").

to_query_test() ->
    ?assertEqual("landing=http%3a%2f%2fstrikead.com&a=1",
        xl_url:to_query([{landing, "http://strikead.com"}, {a, 1}])).


escape_params_test() ->
    ?assertEqual([{a, "a%2bb"}],
        xl_url:escape_params([{a, "a+b"}])).

substitute_test() ->
    ?assertEqual("http://strikead.com?a=a%2bb",
        xl_url:substitute("http://strikead.com?a={a}", [{a, "a+b"}])).

domain_test() ->
    ?assertEqual("strikead.com", xl_url:domain(2, "www.strikead.com")),
    ?assertEqual("localhost", xl_url:domain(2, "localhost")).

