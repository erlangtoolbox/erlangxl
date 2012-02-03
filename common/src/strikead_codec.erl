-module(strikead_codec).
-export([md5/1, list_to_hex/1, binary_to_hex/1]).

md5(L) -> binary_to_hex(md5(L, erlang:md5_init())).

md5([], Ctx) -> erlang:md5_final(Ctx);
md5([H|T], Ctx) -> md5(T, erlang:md5_update(Ctx, H)).

binary_to_hex(B) -> list_to_hex(binary_to_list(B)).

list_to_hex(L) ->  lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- L]).
