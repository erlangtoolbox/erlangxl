%% Copyright
-module(pl_persist_data_orddict).
-author("volodymyr.kyrychenko@strikead.com").

-include("pl_persist.hrl").

-behaviour(pl_persist_data).
%% API
-export([initialize/1, store/2, values/1]).

initialize(Options) ->
    case xl_lists:kvfind(identify, Options) of
        {ok, Fun} ->
            #persist_data{
                    module = ?MODULE,
                    identify = Fun,
                    data = orddict:new()
            };
        undefined -> error(badarg, required_identify)
    end.

store(Data = #persist_data{identify = Id, data = Dict}, Values) when is_list(Values) ->
    Data#persist_data{data = lists:foldl(fun(V, D) -> orddict:store(Id(V), V, D) end, Dict, Values)}.

values(#persist_data{data = Dict}) -> lists:map(fun({_, V}) -> V end, orddict:to_list(Dict)).