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
-module(xl_memdb).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-compile({parse_transform, do}).

%% API
-export([start/1, store/3, store/2, get/2, stop/1, dump/2, load/2, items/1, status/1, updates/2, start/2, update/3]).

-spec(start(atom()) -> ok).
start(Name) -> start(Name, []).

-spec(start(atom(), [term()]) -> ok).
start(Name, Options) ->
    xl_state:new(Name),
    xl_state:set(Name, memory, xl_memdb_memory:new(Name, xl_lists:kvfind(memory, Options, ets), Options)).

-spec(store(atom(), term(), term()) -> error_m:monad(ok)).
store(Name, Key, Value) ->
    do([error_m ||
        Memory <- xl_state:evalue(Name, memory),
        xl_memdb_memory:store(Memory, Key, Value)
    ]).

-spec(store(atom(), [{term(), term()}]) -> ok).
store(Name, List) ->
    do([error_m ||
        Memory <- xl_state:evalue(Name, memory),
        xl_memdb_memory:store(Memory, List)
    ]).

-spec(get(atom(), term()) -> option_m:monad(term())).
get(Name, Key) ->
    do([option_m ||
        Memory <- xl_state:value(Name, memory),
        xl_memdb_memory:get(Memory, Key)
    ]).

-spec(stop(atom()) -> error_m:monad(ok)).
stop(Name) ->
    do([option_m ||
        Memory <- xl_state:value(Name, memory),
        xl_memdb_memory:release(Memory)
    ]).

-spec(dump(atom(), file:filename()) -> error_m:monad(ok)).
dump(Name, Location) ->
    do([option_m ||
        Memory <- xl_state:value(Name, memory),
        xl_memdb_memory:dump(Memory, Location)
    ]).

-spec(load(atom(), file:filename()) -> error_m:monad(ok)).
load(Name, Location) ->
    do([option_m ||
        Memory <- xl_state:value(Name, memory),
        NewMemory <- xl_memdb_memory:load(Memory, Location),
        xl_state:set(Name, memory, NewMemory)
    ]).

-spec(items(atom()) -> [{term(), term()}]).
items(Name) ->
    do([option_m ||
        Memory <- xl_state:value(Name, memory),
        xl_memdb_memory:items(Memory)
    ]).

-spec(status(atom()) -> error_m:monad(term())).
status(Name) ->
    do([error_m ||
        Memory <- xl_state:value(Name, memory),
        return([{memory, xl_memdb_memory:status(Memory)}, {options, xl_state:values(Name)}])
    ]).

-spec(updates(atom(), pos_integer()) -> xl_stream:stream(term())).
updates(Name, Since) ->
    do([option_m ||
        Memory <- xl_state:value(Name, memory),
        xl_memdb_memory:updates(Memory, Since)
    ]).


-spec(update(atom(), term(), fun((term()) -> option_m:monad(term()))) -> error_m:monad(term())).
update(Name, Key, F) ->
    do([option_m ||
        Memory <- xl_state:value(Name, memory),
        xl_memdb_memory:update(Memory, Key, F)
    ]).
