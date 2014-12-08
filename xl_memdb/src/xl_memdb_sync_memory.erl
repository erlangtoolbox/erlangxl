%% =============================================================================
%%  The MIT License (MIT)
%%
%%  Copyright (c) 2014 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy of
%%  this software and associated documentation files (the "Software"), to deal in
%%  the Software without restriction, including without limitation the rights to
%%  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
%%  the Software, and to permit persons to whom the Software is furnished to do so,
%%  subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in all
%%  copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
%%  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
%%  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
%%  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%%  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%% =============================================================================
-module(xl_memdb_sync_memory).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-compile({parse_transform, do}).

-include("xl_memdb_memory.hrl").

-behaviour(xl_memdb_memory).
%% API
-export([store/3, get/2, release/1, reload/2, new/1, store/2, update/3, ets/1, items/1, updates/2]).

-record(memory, {
    ets :: ets:tab(),
    updater :: pid()
}).

-spec(new(atom()) -> #xl_memdb_memory{}).
new(Name) ->
    ETS = xl_ets_server:create(xl_string:join_atom([Name, '_', xl_uid:next()]), [public, named_table, set]),
    #xl_memdb_memory{
        module = ?MODULE,
        data = #memory{
            ets = ETS,
            updater = spawn_link(fun() ->
                process_flag(trap_exit, true),
                update_loop(ETS)
            end)
        }
    }.

-spec(store(#xl_memdb_memory{}, term(), term()) -> error_m:monad(term())).
store(Memory, Key, Value) -> store(Memory, [{Key, Value}]).

-spec(store(#xl_memdb_memory{}, [{term(), term()}]) -> error_m:monad(ok)).
store(#xl_memdb_memory{data = #memory{updater = Updater}}, List) ->
    xl_erlang:send_and_receive(Updater, {mutate, fun(ETS) ->
        try
            true = ets:insert(ETS, lists:map(fun
                ({Key, Value}) -> {Key, Value, xl_calendar:now_micros()};
                (X) when size(X) == 3 -> X
            end, List)),
            ok
        catch
            _ : E -> {error, E}
        end
    end}).

-spec(get(#xl_memdb_memory{}, term()) -> option_m:monad(term())).
get(#xl_memdb_memory{data = #memory{ets = ETS}}, Key) ->
    case xl_ets:lookup_object(ETS, Key) of
        {ok, {Key, Value, _Meta}} -> {ok, Value};
        undefined -> undefined
    end.

-spec(release(#xl_memdb_memory{}) -> error_m:monad(ok)).
release(#xl_memdb_memory{data = #memory{updater = Updater}}) ->
    xl_erlang:send_and_receive(Updater, exit).

-spec(reload(#xl_memdb_memory{}, ets:tab()) -> #xl_memdb_memory{}).
reload(Memory = #xl_memdb_memory{data = #memory{updater = Updater}}, NewETS) ->
    xl_erlang:send_and_receive(Updater, {reload, NewETS}),
    Memory#xl_memdb_memory{
        data = Memory#xl_memdb_memory.data#memory{
            ets = NewETS
        }
    }.

-spec(update(#xl_memdb_memory{}, term(), fun((term()) -> option_m:monad(term()))) -> error_m:monad(term())).
update(#xl_memdb_memory{data = #memory{updater = Updater}}, Key, F) ->
    xl_erlang:send_and_receive(Updater, {mutate, fun(ETS) ->
        do([option_m ||
            V <- case xl_ets:lookup_object(ETS, Key) of
                {ok, {Key, Value, _Meta}} -> return(Value);
                _ -> return(undefined)
            end,
            case F(V) of
                {ok, Result} ->
                    ets:insert(ETS, {Key, Result, xl_calendar:now_micros()}),
                    return(Result);
                undefined ->
                    return(undefined)
            end
        ])
    end}).

-spec(ets(#xl_memdb_memory{}) -> error_m:monad(ets:tab())).
ets(#xl_memdb_memory{data = #memory{updater = Updater}}) -> xl_erlang:send_and_receive(Updater, ets).

-spec(items(#xl_memdb_memory{}) -> [{term(), term()}]).
items(#xl_memdb_memory{data = #memory{ets = ETS}}) ->
    lists:map(fun({Key, Value, _}) -> {Key, Value} end, ets:tab2list(ETS)).

-spec(updates(#xl_memdb_memory{}, pos_integer()) -> xl_stream:stream(term())).
updates(#xl_memdb_memory{data = #memory{ets = ETS}}, Since) ->
    xl_ets:cursor(ETS, fun({_Key, _Value, LastUpdate}) -> LastUpdate > Since end).

%% @hidden
update_loop(ETS) ->
    receive
        {'EXIT', _From, _Reason} ->
            ets:delete(ETS);
        {From, exit} ->
            ets:delete(ETS),
            From ! ok;
        {From, ets} ->
            From ! {ok, ETS},
            update_loop(ETS);
        {From, {mutate, Fun}} ->
            From ! try
                Fun(ETS)
            catch
                _ : E -> {error, E}
            end,
            update_loop(ETS);
        {From, {reload, NewETS}} ->
            case ETS of
                NewETS -> ok;
                _ -> ets:delete(ETS)
            end,
            From ! ok,
            update_loop(NewETS)
    end.

