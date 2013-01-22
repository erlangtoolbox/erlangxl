-module(xl_codec).
-export([md5/1, sha1/1, list_to_hex/1, binary_to_hex/1]).

sha1(undefined) ->
	undefined;
sha1(L) ->
	binary_to_hex(crypto:sha(L)).

md5(undefined) ->
	undefined;
md5(L) ->
	binary_to_hex(crypto:md5(L)).

binary_to_hex(B) -> 
	xl_convert:to(binary, binary_to_hex_(B)).

list_to_hex(L) -> 
	binary_to_hex_(L).

binary_to_hex_(<<>>) ->
    [];
binary_to_hex_(<< N1:4, N2:4, Bin/binary >>) ->
    [num_to_hex(N1), num_to_hex(N2) | binary_to_hex_(Bin)].

num_to_hex(N) when N < 10 ->
    N + $0;
num_to_hex(N) when N > 9 -> 
    N - 10 + $a.

