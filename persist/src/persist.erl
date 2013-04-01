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
-module(persist).

-compile({parse_transform, do}).

-export([open/4, open/3, close/1, store/2, select/1, delete/2, get/2, by_index/1,
    changes/2, identify/2, cursor/1, cursor/2, cursor/3, lookup/2]).

% Internal
-export([ets_changes/2]).

-record(persister, {
    name :: atom(),
    identify :: fun((tuple()) -> xl_string:iostring()),
    ets :: ets:tid() | atom(),
    fsync :: pid(),
    indices :: [ets:tid() | atom()]
}).

-opaque persister() :: #persister{}.
-export_types([persister/0]).

-spec open/3 :: (atom(), fun((term()) -> xl_string:iostring()), atom()) -> error_m:monad(persister()).
open(Name, Identify, StoreModule) -> open(Name, Identify, StoreModule, []).

-spec open/4 :: (atom(), fun((term()) -> xl_string:iostring()), atom(), [{atom(), term()}]) -> error_m:monad(persister()).
open(Name, Identify, StoreModule, Options) ->
    ETS = ets:new(xl_convert:make_atom([Name, '_persister']), [
        ordered_set, {keypos, 1}, public
    ]),
    Indices = lists:map(fun({IndexName, F}) ->
        {IndexName, persist_index:new(IndexName, F, Identify)}
    end, xl_lists:kvfind(indices, Options, [])),
    do([error_m ||
        Objects <- StoreModule:load(),
        return([begin ets:insert(ETS, X), index_index(Indices, O) end || X = {_, O, _, _} <- Objects]),
        Fsync <- persist_fsync:start_link(
            ETS,
            xl_lists:kvfind(fsync_interval, Options, 5000),
            StoreModule
        ),
        return(#persister{
                    name = Name,
                    identify = Identify,
                    ets = ETS,
                    fsync = Fsync,
                    indices = Indices
        })
    ]).

-spec close/1 :: (persister()) -> error_m:monad(ok).
close(#persister{fsync = Fsync}) ->
    persist_fsync:stop(Fsync).

-spec store/2 :: (persister(), term()) -> ok.
store(#persister{ets = ETS, identify = Id, indices = Indices}, X) ->
    true = ets:insert(ETS, {Id(X), X, xl_calendar:now_millis(), false}),
    index_update(Indices, X).

-spec select/1 :: (persister()) -> [term()].
select(#persister{ets = ETS}) ->
    lists:flatten(ets:match(ETS, {'_', '$1', '_', false})).

-spec delete/2 :: (persister(), xl_string:iostring()) -> ok.
delete(#persister{ets = ETS}, Id) ->
    ets:update_element(ETS, Id, [{3, xl_calendar:now_millis()}, {4, true}]),
    ok.

-spec get/2 :: (persister(), xl_string:iostring()) -> option_m:monad(term()).
get(#persister{ets = ETS}, Id) ->
    case ets:lookup(ETS, Id) of
        [{_, _, _, true}] -> undefined;
        [{_, X, _, _}] -> {ok, X};
        _ -> undefined
    end.

-spec by_index/1 :: (pos_integer()) -> fun((term()) -> xl_string:iostring()).
by_index(N) -> fun(X) -> element(N, X) end.

-spec changes/2 :: (persister(), integer()) ->
    xl_stream:stream(term()).
changes(#persister{ets = ETS}, Since) ->
    xl_stream:map(fun({_, X, _, _}) -> X end, ets_changes(ETS, Since)).

-spec identify/2 :: (persister(), term()) -> xl_string:iostring().
identify(#persister{identify = I}, X) -> I(X).

-spec(cursor(persister()) -> xl_stream:stream(term())).
cursor(P) -> cursor(P, []).

-spec(lookup(persister(), [{atom(), term()}]) -> xl_stream:stream(term())).
lookup(P = #persister{indices = Indices}, Query) ->
    {IndexNames, IndexData} = lists:unzip(index_lookup(Indices, Query)),
    case xl_stream:matchfilter(fun persist_index:matchfilter_comparator/2, IndexData) of
        [] -> xl_stream:empty();
        Result ->
            xl_stream:filter(fun(X) -> X /= undefined end,
                xl_stream:map(fun([{_, Id, _} | _] = Values) ->
                    case get(P, Id) of
                        {ok, Obj} -> {Obj, lists:zip(IndexNames, lists:map(fun({_, _, Context}) -> Context end, Values))};
                        _ -> undefined
                    end
                end, Result)
            )
    end.

-spec(cursor(persister(), [term()]) -> xl_stream:stream()).
cursor(#persister{ets = ETS}, Options) ->
    xl_stream:map(fun({_, X, _, _}) -> X end,
        xl_stream:filter(fun({_, _, _, Deleted}) -> not(Deleted) end, ets_cursor(ETS, Options))
    ).

-spec(cursor(persister(), fun((term()) -> boolean()), [term()]) -> xl_stream:stream()).
cursor(P, Predicate, Options) -> xl_stream:filter(Predicate, cursor(P, Options)).

ets_cursor(ETS, Options) ->
    case ets:info(ETS, size) of
        0 -> xl_stream:empty();
        Size ->
            Pos = case lists:member(random, Options) of
                true -> xl_random:uniform(Size) - 1;
                _ -> 0
            end,
            case ets:slot(ETS, Pos) of
                '$end_of_table' -> xl_stream:empty();
                [{MiddleKey, _, _, _}] ->
                    xl_stream:map(fun([X]) -> X end,
                        xl_stream:stream({MiddleKey, MiddleKey}, fun
                            ({'$end_of_table', '$end_of_table'}) -> empty;
                            ({Key1, '$end_of_table'}) ->
                                case ets:prev(ETS, Key1) of
                                    '$end_of_table' -> empty;
                                    X -> {ets:lookup(ETS, X), {X, '$end_of_table'}}
                                end;
                            ({Key1, Key2}) -> {ets:lookup(ETS, Key2), {Key1, ets:next(ETS, Key2)}}
                        end)
                    )
            end
    end.

ets_cursor(ETS, Predicate, Options) -> xl_stream:filter(Predicate, ets_cursor(ETS, Options)).

ets_changes(ETS, Since) -> ets_cursor(ETS, fun({_, _, Date, _}) -> Since =< Date end, []).

index_index(Indices, Object) ->
    lists:foreach(fun({_, Index}) -> persist_index:index(Index, Object) end, Indices).

index_update(Indices, Object) ->
    lists:foreach(fun({_, Index}) -> persist_index:update(Index, Object) end, Indices).

index_lookup(Indices, Query) ->
    lists:map(fun({Name, Value}) ->
        case xl_lists:kvfind(Name, Indices) of
            {ok, Index} -> {Name, persist_index:lookup(Index, Value)};
            undefined -> error({unknown_index, Name})
        end
    end, Query).

