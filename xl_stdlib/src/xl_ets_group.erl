%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @copyright (C) 2014, strikead.com
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xl_ets_group).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-compile({parse_transform, do}).

%% API
-export([start/1, get_ets/2, new_ets/3, clear/1, stop/1, new_ets/2]).

-spec(start(atom()) -> ok).
start(Name) -> xl_state:new(Name, [bag]).

-spec(new_ets(atom(), atom()) -> ets:tab()).
new_ets(Name, EtsName) ->
    new_ets(Name, EtsName, []).

-spec(new_ets(atom(), atom(), [term()]) -> ets:tab()).
new_ets(Name, EtsName, EtsOptions) ->
    Ets = xl_ets_server:create(EtsName, EtsOptions),
    xl_state:set(Name, tables, {EtsName, Ets}),
    Ets.

-spec(get_ets(atom(), atom()) -> option_m:monad(ets:tab())).
get_ets(Name, EtsName) ->
    do([option_m ||
        Tables <- xl_state:get(Name, tables),
        xl_lists:kvfind(EtsName, Tables)
    ]).

-spec(clear(atom()) -> ok).
clear(Name) ->
    case xl_state:get(Name, tables) of
        {ok, Tables} ->
            lists:foreach(fun({_, T}) -> xl_ets_server:delete(T) end, Tables),
            xl_state:remove(Name, tables);
        _ -> ok
    end.

-spec(stop(atom()) -> ok).
stop(Name) ->
    clear(Name),
    xl_state:delete(Name).
