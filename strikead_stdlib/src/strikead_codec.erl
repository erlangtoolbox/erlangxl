-module(strikead_codec).
-export([md5/1, sha1/1, list_to_hex/1, binary_to_hex/1]).

sha1(L) -> binary_to_hex(crypto:sha(L)).

md5(L) -> binary_to_hex(crypto:md5(L)).

binary_to_hex(B) -> list_to_hex(binary_to_list(B)).

list_to_hex(L) ->  lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- L]).

