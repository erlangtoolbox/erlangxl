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
-module(persist_index).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([new/3, index/2, lookup/2, update/2, matchfilter_comparator/2]).

-export_type([index_fun/0, ident_fun/0]).

-type(index_value() :: {term(), term(), term()}).
-type(index_fun() :: fun((tuple()) -> [index_value()])).
-type(ident_fun() :: fun((tuple()) -> term())).

-record(persist_index, {
    ets :: ets:did() | atom(),
    index :: index_fun(),
    ident :: ident_fun()
}).

-spec(new(atom(), index_fun(), ident_fun()) -> #persist_index{}).
new(Name, Index, Ident) ->
    #persist_index{
            ets = ets:new(xl_convert:make_atom([Name, '_persister_index']), [
                bag, {keypos, 1}, public
            ]),
            index = Index,
            ident = Ident
    }.

-spec(index(#persist_index{}, tuple()) -> ok).
index(#persist_index{ets = ETS, index = Indexer}, Value) ->
    lists:foreach(fun(V) -> ets:insert(ETS, V) end, Indexer(Value)).

-spec(update(#persist_index{}, tuple()) -> ok).
update(I = #persist_index{ets = ETS, ident = Ident}, Value) ->
    ets:match_delete(ETS, {'_', Ident(Value), '_'}),
    index(I, Value).

-spec(lookup(#persist_index{}, term()) -> [index_value()]).
lookup(#persist_index{ets = ETS}, Key) ->
    lists:sort(fun({_, Id1, _}, {_, Id2, _}) -> Id1 < Id2 end, ets:lookup(ETS, Key) ++ ets:lookup(ETS, any)).

matchfilter_comparator({_, X, _}, {_, X, _}) -> eq;
matchfilter_comparator({_, X, _}, {_, Y, _}) when X > Y -> gt;
matchfilter_comparator(_, _) -> lt.