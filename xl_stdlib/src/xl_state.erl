%% Copyright
-module(xl_state).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([new/1, new/2, get/2, set/3, keys/1, start_link/0]).

start_link() ->
    Pid = spawn_link(fun loop/0),
    register(?MODULE, Pid).

-spec new/1 :: (atom()) -> atom().
new(Name) -> new(Name, []).

-spec new/2 :: (atom(), [term()]) -> atom().
new(Name, Options) ->
    ?MODULE ! {init, Name, Options, self()},
    receive
        X -> X
    end.

loop() ->
    receive
        {init, Name, Options, Sender} ->
            try
                catch ets:delete(Name),
                ets:new(Name, [named_table, public | Options]),
                Sender ! ok,
                loop()
            catch
                _:E -> Sender ! {error, E}
            end;
        stop -> ok
    end.

-spec keys/1 :: (atom()) -> [term()].
keys(Name) -> lists:flatten(ets:match(Name, {'$1', '_'})).

-spec get/2 :: (atom(), term()) -> option_m:monad(term()).
get(Name, Key) ->
    case ets:lookup(Name, Key) of
        [] -> undefined;
        List -> {ok, lists:map(fun({_Key, Value}) -> Value end, List)}
    end.

-spec set/3 :: (atom(), term(), term()) -> ok.
set(Name, Key, Value) ->
    ets:insert(Name, {Key, Value}),
    ok.
