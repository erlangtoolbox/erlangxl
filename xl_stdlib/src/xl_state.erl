%% Copyright
-module(xl_state).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([new/2, get/2, set/3]).

-spec new/2 :: (atom(), [term()]) -> atom().
new(Name, Options) ->
    Pid = spawn(fun loop/0),
    Pid ! {init, Name, Options, self()},
    receive
        {error, {E, Pid}} -> {error, E};
        {ok, Pid} ->
            RegName = xl_convert:make_atom([?MODULE, Name]),
            register(RegName, Pid),
            {ok, RegName}
    end.

loop() ->
    receive
        {init, Name, Options, Sender} ->
            try
                    catch ets:delete(Name),
                ets:new(Name, [named_table, public | Options]),
                Sender ! {ok, self()},
                loop()
            catch
                _:E ->
                    Sender ! {error, {E, self()}}
            end;
        stop ->
            ok
    end.

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