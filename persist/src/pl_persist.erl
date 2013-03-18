%% Copyright
-module(pl_persist).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([open/2, mutate/2, read/2]).

-record(persistor, {
    pid
}).
open(Identify, Options) ->
    Backend = xl_lists:kvfind(data_backend, Options, persist2_data_orddict),
    Data = Backend:initialize(Identify, Options),
    Pid = spawn_link(fun() -> loop(Data) end),
    #persistor{
            pid = Pid
    }.

loop(Data) ->
    receive
        {mutate, Fun, From} ->
            case Fun(Data) of
                {NewData} ->
                    From ! {result, ok},
                    loop(NewData);
                {Result, NewData} ->
                    From ! {result, Result},
                    loop(NewData)
            end;
        {read, Fun, From} ->
            spawn(fun() ->
                From ! {result, Fun(Data)}
            end),
            loop(Data)
    end.

mutate(#persistor{pid = Pid}, Fun) ->
    Pid ! {mutate, Fun, self()},
    receive
        {result, R} -> R
    end.

read(#persistor{pid = Pid}, Fun) ->
    Pid ! {read, Fun, self()},
    receive
        {result, R} -> R
    end.
