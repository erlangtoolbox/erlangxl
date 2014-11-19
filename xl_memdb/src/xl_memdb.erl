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
-export([start/1, store/3, store/2, get/2, stop/1, dump/2, load/2, items/1, status/1, updates/2]).

-spec(start(atom()) -> ok).
start(Name) ->
    do([error_m ||
        xl_state:new(Name),
        xl_state:set(Name, ets, create_ets(Name))
    ]).

-spec(store(atom(), term(), term()) -> ok).
store(Name, Key, Value) ->
    {ok, ETS} = xl_state:value(Name, ets),
    true = ets:insert(ETS, {Key, Value, xl_calendar:now_micros()}),
    ok.

-spec(store(atom(), [{term(), term()}]) -> ok).
store(Name, List) ->
    {ok, ETS} = xl_state:value(Name, ets),
    true = ets:insert(ETS, List),
    ok.

-spec(get(atom(), term()) -> option_m:monad(term())).
get(Name, Key) ->
    do([option_m ||
        ETS <- xl_state:value(Name, ets),
        O <- xl_ets:lookup_object(ETS, Key),
        case O of
            {Key, Value} -> return(Value);
            {Key, Value, _Meta} -> return(Value)
        end
    ]).

-spec(stop(atom()) -> ok).
stop(Name) ->
    case xl_state:value(Name, ets) of
        {ok, ETS} -> ets:delete(ETS);
        undefined -> ok
    end,
    xl_state:delete(Name).

-spec(dump(atom(), file:filename()) -> error_m:monad(ok)).
dump(Name, Location) ->
    do([error_m ||
        ETS <- option_m:to_error_m(xl_state:value(Name, ets), no_ets_in_memory),
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
        case ets:insert(ETS, List) of
            true -> {ok, ETS};
            false -> {error, {"cannot store data to table", Name, List}}
        end
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
    do([error_m ||
        OldETS <- option_m:to_error_m(xl_state:value(Name, ets), no_ets_in_memory),
        NewETS <- Fun(),
        xl_state:set(Name, ets, NewETS),
        case OldETS of
            NewETS -> ok;
            _ -> return(ets:delete(OldETS))
        end
    ]).

-spec(items(atom()) -> [{term(), term()}]).
items(Name) ->
    case xl_state:value(Name, ets) of
        {ok, ETS} ->
            lists:map(fun
                (KV = {_, _}) -> KV;
                ({Key, Value, _}) -> {Key, Value}
            end, ets:tab2list(ETS));
        _ -> []
    end.

-spec(status(atom()) -> term()).
status(Name) ->
    {ok, ETS} = xl_state:value(Name, ets),
    ets:info(ETS).

-spec(updates(atom(), pos_integer()) -> xl_stream:stream(term())).
updates(Name, Since) ->
    {ok, ETS} = xl_state:evalue(Name, ets),
    xl_ets:cursor(ETS, fun
        ({_Key, _Value}) -> true;
        ({_Key, _Value, LastUpdate}) -> LastUpdate > Since
    end).


%% @hidden
create_ets(Name) ->
    xl_ets_server:create(
        xl_string:join_atom([Name, '_', xl_uid:next()]),
        [public, named_table, set]).
