%% Copyright
-module(xl_scheduler).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

%% API
-export([start/0, interval/5, cancel/1, apply/5, apply_with_state/6, interval_with_state/6]).

-spec(start() -> error_m:monad(ok)).
start() -> xl_state:new(?MODULE).

-spec(interval(atom(), pos_integer(), module(), atom(), [term()]) -> error_m:monad(ok)).
interval(Name, Interval, M, F, A) ->
    do([error_m ||
        TRef <- timer:apply_after(Interval, ?MODULE, apply, [Name, Interval, M, F, A]),
        xl_state:set(?MODULE, Name, TRef)
    ]).

-spec(interval_with_state(atom(), pos_integer(), module(), atom(), [term()], term()) -> error_m:monad(ok)).
interval_with_state(Name, Interval, M, F, A, State) ->
    do([error_m ||
        TRef <- timer:apply_after(Interval, ?MODULE, apply_with_state, [Name, Interval, M, F, A, State]),
        xl_state:set(?MODULE, Name, TRef)
    ]).

%% @hidden
apply(Name, Interval, M, F, A) ->
    apply(M, F, A),
    case xl_state:get(?MODULE, Name) of
        {ok, _} -> interval(Name, Interval, M, F, A);
        undefined -> ok
    end.

%% @hidden
apply_with_state(Name, Interval, M, F, A, State) ->
    NewState = apply(M, F, A ++ [State]),
    case xl_state:get(?MODULE, Name) of
        {ok, _} -> interval_with_state(Name, Interval, M, F, A, NewState);
        undefined -> ok
    end.

-spec(cancel(atom()) -> error_m:monad(ok)).
cancel(Name) ->
    case xl_state:value(?MODULE, Name) of
        {ok, TRef} ->
            xl_state:remove(?MODULE, Name),
            timer:cancel(TRef);
        undefined -> {error, undefined}
    end.
