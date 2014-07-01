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

-spec(load(atom(), list({term(), term()}) | file:filename()) -> error_m:monad(ok)).
load(Name, []) ->
    load_list(Name, []);
load(Name, List = [{_, _} | _]) ->
    load_list(Name, List);
load(Name, Location) ->
    load_file(Name, Location).

-spec(load_list(atom(), list({term(), term()})) -> error_m:monad(ok)).
load_list(Name, List) ->
    replace(Name, fun() ->
        ETS = create_ets(Name),
        true = ets:insert(ETS, List),
        {ok, ETS}
    end).

-spec(load_file(atom(), file:filename()) -> error_m:monad(ok)).
load_file(Name, Location) ->
    replace(Name, fun() ->
        do([error_m ||
            ETS <- ets:file2tab(Location),
            xl_ets_server:takeover(ETS),
            return(ETS)
        ])
    end).

-spec(replace(atom(), fun(() -> error_m:monad(ets:tab()))) -> error_m:monad(ok)).
replace(Name, Fun) ->
    {ok, OldETS} = xl_state:value(Name, ets),
    do([error_m ||
        NewETS <- Fun(),
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
