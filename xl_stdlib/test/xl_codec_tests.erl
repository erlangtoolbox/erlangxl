-module(xl_codec_tests).
-autor("Roman Dayneko <roman.dayneko@strikead.com>").

-include_lib("eunit/include/eunit.hrl").

-define(DATA, <<"test">>).
-define(DATAMD5, <<"098f6bcd4621d373cade4e832627b4f6">>).

md5_as_bin_hex_test() ->
	?assertEqual(?DATAMD5, xl_codec:md5(?DATA)).

md5_as_string_hex_test() ->
	erlang:display({test, (catch xl_codec:list_to_hex(crypto:md5(xl_convert:to(string, ?DATA))))}),
	?assertEqual(xl_convert:to(string, ?DATAMD5), xl_codec:list_to_hex(crypto:md5(xl_convert:to(string, ?DATA)))).
