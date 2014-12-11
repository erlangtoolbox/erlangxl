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
-module(xl_memdb_memory).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-include("xl_memdb_memory.hrl").

%% API
-export([store/3, release/1, get/2, store/2, update/3, new/3, items/1, updates/2, load/2, dump/2, status/1]).

-callback(store(#xl_memdb_memory{}, term(), term()) -> error_m:monad(term())).
-callback(store(#xl_memdb_memory{}, [{term(), term()}]) -> error_m:monad(ok)).
-callback(get(#xl_memdb_memory{}, term()) -> option_m:monad(term())).
-callback(release(#xl_memdb_memory{}) -> error_m:monad(ok)).
-callback(update(#xl_memdb_memory{}, term(), fun((term()) -> option_m:monad(term()))) -> error_m:monad(term())).
-callback(items(#xl_memdb_memory{}) -> [{term(), term()}]).
-callback(updates(#xl_memdb_memory{}, pos_integer()) -> xl_stream:stream(term())).
-callback(load(#xl_memdb_memory{}, file:name()) -> error_m:monad(ok)).
-callback(dump(#xl_memdb_memory{}, file:name()) -> error_m:monad(ok)).
-callback(status(#xl_memdb_memory{}) -> term()).

-spec(store(#xl_memdb_memory{}, term(), term()) -> error_m:monad(term())).
store(Memory = #xl_memdb_memory{module = Module}, Key, Value) -> Module:store(Memory, Key, Value).

-spec(store(#xl_memdb_memory{}, [{term(), term()}]) -> error_m:monad(ok)).
store(Memory = #xl_memdb_memory{module = Module}, List) -> Module:store(Memory, List).

-spec(get(#xl_memdb_memory{}, term()) -> option_m:monad(term())).
get(Memory = #xl_memdb_memory{module = Module}, Key) -> Module:get(Memory, Key).

-spec(release(#xl_memdb_memory{}) -> error_m:monad(ok)).
release(Memory = #xl_memdb_memory{module = Module}) -> Module:release(Memory).

-spec(update(#xl_memdb_memory{}, term(), fun((term()) -> option_m:monad(term()))) -> error_m:monad(term())).
update(Memory = #xl_memdb_memory{module = Module}, Key, F) -> Module:update(Memory, Key, F).

-spec(status(#xl_memdb_memory{}) -> error_m:monad(term())).
status(Memory = #xl_memdb_memory{module = Module}) -> Module:status(Memory).

-spec(new(atom(), atom(), term()) -> #xl_memdb_memory{}).
new(Name, tdb, _Options) -> xl_memdb_tdb_memory:new(Name);
new(Name, ets, _Options) -> xl_memdb_ets_memory:new(Name);
new(Name, rets, Options) -> xl_memdb_rets_memory:new(Name, Options).

-spec(items(#xl_memdb_memory{}) -> [{term(), term()}]).
items(Memory = #xl_memdb_memory{module = Module}) -> Module:items(Memory).

-spec(updates(#xl_memdb_memory{}, pos_integer()) -> xl_stream:stream(term())).
updates(Memory = #xl_memdb_memory{module = Module}, Since) -> Module:updates(Memory, Since).

-spec(load(#xl_memdb_memory{}, file:name()) -> error_m:monad(ok)).
load(Memory = #xl_memdb_memory{module = Module}, Location) -> Module:load(Memory, Location).

-spec(dump(#xl_memdb_memory{}, file:name()) -> error_m:monad(ok)).
dump(Memory = #xl_memdb_memory{module = Module}, Location) -> Module:dump(Memory, Location).
