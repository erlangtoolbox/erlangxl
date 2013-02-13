-module(persist).

-compile({parse_transform, do}).

-export([open/4, open/3, close/1, store/2, select/1, delete/2, get/2, by_index/1,
    changes/2, identify/2, cursor/1, cursor/2]).

% Internal
-export([ets_changes/2]).

-record(persister, {
    name :: atom(),
    identify :: fun((tuple()) -> xl_string:iostring()),
    ets :: ets:tid() | atom(),
    fsync :: pid()
}).

-opaque persister() :: #persister{}.
-export_types([persister/0]).

-spec open/3 :: (atom(), fun((term()) -> xl_string:iostring()), atom()) ->
    error_m:monad(persister()).
open(Name, Identify, StoreModule) -> open(Name, Identify, StoreModule, []).

-spec open/4 :: (atom(), fun((term()) -> xl_string:iostring()), atom(), [{atom(), term()}]) ->
    error_m:monad(persister()).
open(Name, Identify, StoreModule, Options) ->
    ETS = ets:new(xl_convert:make_atom([Name, '_persister']), [
        ordered_set, {keypos, 1}, protected
    ]),
    do([error_m ||
        Objects <- StoreModule:load(),
        return([ets:insert(ETS, X) || X <- Objects]),
        Fsync <- persist_fsync:start_link(
            ETS,
            xl_lists:kvfind(fsync_interval, Options, 5000),
            xl_lists:kvfind(fsync_delete, Options, true),
            StoreModule
        ),
        return(#persister{
                    name = Name,
                    identify = Identify,
                    ets = ETS,
                    fsync = Fsync
        })
    ]).

-spec close/1 :: (persister()) -> error_m:monad(ok).
close(#persister{fsync = Fsync}) ->
    persist_fsync:stop(Fsync).

-spec store/2 :: (persister(), term()) -> ok.
store(#persister{ets = ETS, identify = Id}, X) ->
    true = ets:insert(ETS, {Id(X), X, xl_calendar:now_millis(), false}),
    ok.

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
    [{update | delete, term() | binary()}].
changes(#persister{ets = ETS}, Since) ->
    lists:map(fun
        ({Id, _, _, true}) -> {delete, Id};
        ({_, X, _, false}) -> {update, X}
    end, ets_changes(ETS, Since)).

-spec identify/2 :: (persister(), term()) -> xl_string:iostring().
identify(#persister{identify = I}, X) -> I(X).

ets_changes(ETS, Since) ->
    ets:select(ETS, [{
        {'$1', '$2', '$3', '$4'},
        [{'=<', Since, '$3'}],
        ['$_']
    }]).

-spec(cursor(persister()) -> xl_stream:stream()).
cursor(P) -> cursor(P, []).

-spec(cursor(persister(), [term()]) -> xl_stream:stream()).
cursor(#persister{ets = ETS, identify = Id}, Options) ->
    case ets:info(ETS, size) of
        0 -> xl_stream:empty();
        Size ->
            Pos = case lists:member(random, Options) of
                true -> xl_random:uniform(Size) - 1;
                _ -> 0
            end,
            case ets:slot(ETS, Pos) of
                '$end_of_table' -> xl_stream:empty();
                [{_, Object, _, _}] ->
                    MiddleKey = Id(Object),
                    xl_stream:map(fun([{_, X, _, _}]) -> X end,
                        xl_stream:filter(fun([{_, _, _, Deleted}]) -> not(Deleted) end,
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
                    )
            end
    end.