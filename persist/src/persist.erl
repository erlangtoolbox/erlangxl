-module(persist).

-compile({parse_transform, do}).

-export([open/4, open/3, close/1, store/2, select/1, delete/2, get/2, by_index/1]).

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
    ETS = ets:new(xl_string:mk_atom([Name, '_persister']), [
        ordered_set, {keypos, 1}, protected
    ]),
    do([error_m ||
        Objects <- StoreModule:load(),
        return([ets:insert(ETS, {Identify(X), X, 0, false}) || X <- Objects]),
        Fsync <- persist_fsync:start_link(
            ETS,
            xl_lists:kvfind(fsync_interval, Options, 5000),
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