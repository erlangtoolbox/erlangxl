%% Copyright
-module(xl_base64url_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").
-include("xl_eunit.hrl").

encode_test() ->
    ?assertEquals(<<"aHR0cDovL3N0cmlrZWFkLmNvbS94P3k9eiZmPWYrNA">>, xl_base64url:encode("http://strikead.com/x?y=z&f=f+4")),
    xl_eunit:performance(b64_encode, fun(_) ->
        xl_base64url:encode("http://strikead.com/x?y=z&f=f+4")
    end, 100000).

b64e_test() ->
    lists:foreach(fun(X) ->
        ?assertEqual(xl_base64url:b64e_old(X), xl_base64url:b64e(X))
    end, lists:seq(0, 63)).
