%% Copyright
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
    lists:sort(fun({_, Id1, _}, {_, Id2, _}) -> Id1 < Id2 end, ets:lookup(ETS, Key)).

matchfilter_comparator({_, X, _}, {_, X, _}) -> eq;
matchfilter_comparator({_, X, _}, {_, Y, _}) when X > Y -> gt;
matchfilter_comparator(_, _) -> lt.