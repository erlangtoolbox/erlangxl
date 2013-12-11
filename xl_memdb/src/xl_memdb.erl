%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @copyright (C) 2013, strikead.com
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xl_memdb).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-compile({parse_transform, do}).

%% API
-export([start/1, store/3, store/2, get/2, stop/1, dump/2, load/2, items/1]).

-spec(start(atom()) -> ok).
start(Name) ->
    do([error_m ||
        xl_state:new(Name),
        xl_state:set(Name, ets, create_ets(Name))
    ]).

-spec(store(atom(), term(), term()) -> ok).
store(Name, Key, Value) ->
    {ok, ETS} = xl_state:value(Name, ets),
    true = ets:insert(ETS, {Key, Value}),
    ok.

-spec(store(atom(), [{term(), term()}]) -> ok).
store(Name, List) ->
    {ok, ETS} = xl_state:value(Name, ets),
    true = ets:insert(ETS, List),
    ok.

-spec(get(atom(), term()) -> option_m:monad(term())).
get(Name, Key) ->
    {ok, ETS} = xl_state:value(Name, ets),
    xl_ets:lookup_object(ETS, Key).

-spec(stop(atom()) -> ok).
stop(Name) ->
    {ok, ETS} = xl_state:value(Name, ets),
    ets:delete(ETS),
    xl_state:delete(Name).

-spec(dump(atom(), file:filename()) -> error_m:monad(ok)).
dump(Name, Location) ->
    {ok, ETS} = xl_state:value(Name, ets),
    do([error_m ||
        xl_file:ensure_dir(Location),
        ets:tab2file(ETS, Location)
    ]).

-spec(load(atom(), file:filename()) -> error_m:monad(ok)).
load(Name, Location) ->
    {ok, ETS} = xl_state:value(Name, ets),
    ets:delete(ETS),
    do([error_m ||
        Tab <- ets:file2tab(Location),
        xl_state:set(Name, ets, Tab)
    ]).

items(Name) ->
    {ok, ETS} = xl_state:value(Name, ets),
    ets:tab2list(ETS).

%% @hidden
create_ets(Name) ->
    xl_ets_server:create(
        xl_convert:make_atom([Name, '_', ets]),
        [public, named_table, set]).
