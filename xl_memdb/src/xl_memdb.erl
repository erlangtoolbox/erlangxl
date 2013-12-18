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
-export([start/1, store/3, store/2, get/2, stop/1, dump/2, load/2, items/1, status/1]).

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
    {ok, OldETS} = xl_state:value(Name, ets),
    do([error_m ||
        NewETS <- ets:file2tab(Location),
        xl_ets_server:takeover(NewETS),
        xl_state:set(Name, ets, NewETS),
        case OldETS of
            NewETS -> ok;
            _ -> return(ets:delete(OldETS))
        end
    ]).

items(Name) ->
    {ok, ETS} = xl_state:value(Name, ets),
    ets:tab2list(ETS).

status(Name) ->
    {ok, ETS} = xl_state:value(Name, ets),
    ets:info(ETS).

%% @hidden
create_ets(Name) ->
    xl_ets_server:create(
        xl_convert:make_atom([Name, '_', xl_uid:next()]),
        [public, named_table, set]).
