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
-module(xl_memdb_rets_memory).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-compile({parse_transform, do}).

-include("xl_memdb_memory.hrl").

-behaviour(xl_memdb_memory).

%% API
-export([new/2, store/3, get/2, release/1, store/2, update/3, items/1, updates/2, load/2, dump/2, status/1]).

-record(memory, {
    ets :: ets:tab(),
    rsync :: atom()
}).

-spec(new(atom(), term()) -> #xl_memdb_memory{}).
new(Name, Options) ->
    RSyncName = xl_string:join_atom([Name, rsync]),
    Memory = #xl_memdb_memory{
        module = ?MODULE,
        memory = #memory{
            ets = xl_ets_server:create(xl_string:join_atom([Name, '_', xl_uid:next()]), [public, named_table, set]),
            rsync = RSyncName
        }
    },
    xl_memdb_rsync:start(RSyncName, Options, Memory),
    Memory.

-spec(release(#xl_memdb_memory{}) -> error_m:monad(ok)).
release(#xl_memdb_memory{memory = #memory{ets = ETS, rsync = Name}}) ->
    try
        xl_memdb_rsync:stop(Name),
        true = ets:delete(ETS),
        ok
    catch
        _ : E -> {error, E}
    end.

-spec(store(#xl_memdb_memory{}, term(), term()) -> error_m:monad(term())).
store(Memory, Key, Value) -> store(Memory, [{Key, Value}]).

-spec(store(#xl_memdb_memory{}, [{term(), term()}]) -> error_m:monad(ok)).
store(#xl_memdb_memory{memory = #memory{ets = ETS}}, List) ->
    try
        true = ets:insert(ETS, lists:map(fun
            ({Key, Value}) -> {Key, Value, xl_calendar:now_micros()};
            (X) when size(X) == 3 -> X
        end, List)),
        ok
    catch
        _ : E -> {error, E}
    end.

-spec(get(#xl_memdb_memory{}, term()) -> option_m:monad(term())).
get(#xl_memdb_memory{memory = #memory{ets = ETS}}, Key) ->
    case xl_ets:lookup_object(ETS, Key) of
        {ok, {Key, Value, _LastUpdate}} -> {ok, Value};
        undefined -> undefined
    end.

-spec(update(#xl_memdb_memory{}, term(), fun((term()) -> option_m:monad(term()))) -> error_m:monad(term())).
update(#xl_memdb_memory{memory = #memory{ets = ETS}}, Key, F) ->
    V = case xl_ets:lookup_object(ETS, Key) of
        {ok, {Key, Value, _LastUpdate}} -> Value;
        _ -> undefined
    end,
    try F(V) of
        {ok, Result} ->
            ets:insert(ETS, {Key, Result, xl_calendar:now_micros()}),
            {ok, Result};
        undefined ->
            {ok, undefined}
    catch
        _ : E -> {error, E}
    end.

-spec(items(#xl_memdb_memory{}) -> [{term(), term()}]).
items(#xl_memdb_memory{memory = #memory{ets = ETS}}) ->
    lists:map(fun({Key, Value, _LastUpdate}) -> {Key, Value} end, ets:tab2list(ETS)).

-spec(updates(#xl_memdb_memory{}, pos_integer()) -> xl_stream:stream(term())).
updates(#xl_memdb_memory{memory = #memory{ets = ETS}}, Since) ->
    xl_ets:cursor(ETS, fun({_Key, _Value, LastUpdate}) -> LastUpdate > Since end).

-spec(load(#xl_memdb_memory{}, file:name()) -> error_m:monad(ok)).
load(Memory = #xl_memdb_memory{memory = #memory{ets = ETS}}, Location) ->
    do([error_m ||
        Loaded <- ets:file2tab(Location),
        xl_ets_server:takeover(Loaded),
        case ETS of
            Loaded -> ok;
            _ -> ets:delete(ETS), ok
        end,
        return(Memory#xl_memdb_memory{
            memory = Memory#xl_memdb_memory.memory#memory{
                ets = Loaded
            }
        })
    ]).

-spec(dump(#xl_memdb_memory{}, file:name()) -> error_m:monad(ok)).
dump(#xl_memdb_memory{memory = #memory{ets = ETS}}, Location) ->
    do([error_m ||
        xl_file:ensure_dir(Location),
        ets:tab2file(ETS, Location)
    ]).

-spec(status(#xl_memdb_memory{}) -> term()).
status(#xl_memdb_memory{memory = #memory{ets = ETS}}) -> ets:info(ETS).