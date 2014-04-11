%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(xl_codec).
-export([md5/1, sha1/1, list_to_hex/1, binary_to_hex/1, hex_to_binary/1]).

sha1(undefined) -> undefined;
sha1(L) -> binary_to_hex(crypto:hash(sha, L)).

md5(undefined) -> undefined;
md5(L) -> binary_to_hex(crypto:hash(md5, L)).

binary_to_hex(B) -> list_to_binary(binary_to_hex_(B)).

list_to_hex(L) -> binary_to_hex_(L).

binary_to_hex_(<<>>) -> [];
binary_to_hex_(<<N1:4, N2:4, Bin/binary>>) -> [num_to_hex(N1), num_to_hex(N2) | binary_to_hex_(Bin)].

num_to_hex(N) when N < 10 -> N + $0;
num_to_hex(N) when N > 9 -> N - 10 + $a.

hex_to_binary(<<"0x", S/binary>>) -> hex_to_binary(binary_to_list(S));
hex_to_binary(S) when is_binary(S) -> hex_to_binary(binary_to_list(S));
hex_to_binary(S) -> list_to_binary(hex_to_list(S)).

hex_to_list([X, Y | T]) -> [int(X) * 16 + int(Y) | hex_to_list(T)];
hex_to_list([]) -> [].

int(C) when $0 =< C, C =< $9 -> C - $0;
int(C) when $A =< C, C =< $F -> C - $A + 10;
int(C) when $a =< C, C =< $f -> C - $a + 10.
