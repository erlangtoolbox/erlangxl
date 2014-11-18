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
-module(xl_convert_tests).

-include_lib("eunit/include/eunit.hrl").

-export([convert/1]).

to_float_test() ->
    ?assertEqual(0.0, xl_convert:to(float, "0")),
    ?assertEqual(0.0, xl_convert:to(float, "0.0")).

to_string_test() ->
    ?assertEqual("1", xl_convert:to(string, 1)),
    ?assertEqual("2.2", xl_convert:to(string, 2.2)),
    ?assertEqual("true", xl_convert:to(string, true)),
    ?assertEqual("x", xl_convert:to(string, "x")),
    ?assertEqual("Y", xl_convert:to(string, <<"Y">>)).

to_atom_test() ->
    ?assertEqual('AB', xl_convert:to(atom, "AB")).

to_binary_test() ->
    ?assertEqual(<<"1">>, xl_convert:to(binary, 1)),
    ?assertEqual(<<"str">>, xl_convert:to(binary, "str")).

to_extension_test() ->
    ?assertEqual({fine, a}, xl_convert:to(tests, a)).

convert(X) -> {fine, X}.

