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
-module(persist_index_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

-record(test, {id, name}).
index_test() ->
    Index = persist_index:new(name, fun
        (#test{id = Id, name = []}) -> [{any, Id, Id}];
        (#test{id = Id, name = Names}) -> [{Name, Id, Id} || Name <- Names]
    end, fun(#test{id = Id}) -> Id end),
    persist_index:index(Index, #test{id = 1, name = [a]}),
    persist_index:index(Index, #test{id = 2, name = [b]}),
    persist_index:index(Index, #test{id = 3, name = [a]}),
    persist_index:index(Index, #test{id = 4, name = [a, c, d]}),
    persist_index:index(Index, #test{id = 5, name = []}),
    persist_index:index(Index, #test{id = 6, name = []}),
    ?assertEqual([{a, 1, 1}, {a, 3, 3}, {a, 4, 4}, {any, 5, 5}, {any, 6, 6}], persist_index:lookup(Index, a)),
    ?assertEqual([{b, 2, 2}, {any, 5, 5}, {any, 6, 6}], persist_index:lookup(Index, b)),
    ?assertEqual([{c, 4, 4}, {any, 5, 5}, {any, 6, 6}], persist_index:lookup(Index, c)),
    ?assertEqual([{d, 4, 4}, {any, 5, 5}, {any, 6, 6}], persist_index:lookup(Index, d)),
    ?assertEqual([{any, 5, 5}, {any, 6, 6}], persist_index:lookup(Index, x)),
    persist_index:update(Index, #test{id = 4, name = [e, d, f]}),
    persist_index:update(Index, #test{id = 5, name = [e, d]}),
    ?assertEqual([{a, 1, 1}, {a, 3, 3}, {any, 6, 6}], persist_index:lookup(Index, a)),
    ?assertEqual([{d, 4, 4}, {d, 5, 5}, {any, 6, 6}], persist_index:lookup(Index, d)),
    ?assertEqual([{e, 4, 4}, {e, 5, 5}, {any, 6, 6}], persist_index:lookup(Index, e)),
    ?assertEqual([{f, 4, 4}, {any, 6, 6}], persist_index:lookup(Index, f)).

