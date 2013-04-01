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
-module(epath_lexer_tests).
-author("Volodymyr Kyrychenko <volodymyr.kyrychenko@strikead.com>").

-include_lib("eunit/include/eunit.hrl").

element_test() ->
    ?assertEqual({ok, [
        {element, 1, 1},
        {element, 1, 2}
    ]}, epath_lexer:parse("/$1/$2")).

condition_test() ->
    ?assertEqual({ok, [
        {element, 1, 1},
        {'[', 1},
        {element, 1, 2},
        {cmp, 1, '=='},
        {atom, 1, app},
        {']', 1}
    ]}, epath_lexer:parse("/$1/[$2 == app]")).

condition_quoted_atom_test() ->
    ?assertEqual({ok, [
        {element, 1, 1},
        {'[', 1},
        {element, 1, 2},
        {cmp, 1, '=='},
        {atom, 1, 'a@pp.1'},
        {']', 1}
    ]}, epath_lexer:parse("/$1/[$2 == 'a@pp.1']")).

condition_not_test() ->
    ?assertEqual({ok, [
        {element, 1, 1},
        {'[', 1},
        {element, 1, 2},
        {cmp, 1, '/='},
        {atom, 1, app},
        {']', 1}
    ]}, epath_lexer:parse("/$1/[$2 /= app]")).

string_test() ->
    ?assertEqual({ok, [
        {'[', 1},
        {element, 1, 2},
        {cmp, 1, '=='},
        {string, 1, "a\"pp"},
        {']', 1}
    ]}, epath_lexer:parse("/[$2 == \"a\\\"pp\"]")).

binary_test() ->
    ?assertEqual({ok, [
        {'[', 1},
        {element, 1, 2},
        {cmp, 1, '=='},
        {binary, 1, <<"app">>},
        {']', 1}
    ]}, epath_lexer:parse("/[$2 == <<\"app\">>]")).
