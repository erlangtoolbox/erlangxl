%% Copyright
-module(pl_persist_data).
-author("volodymyr.kyrychenko@strikead.com").

-include("pl_persist.hrl").

%% API
-export([store/2, values/1]).

-callback(initialize([{atom(), term()}]) -> #persist_data{}).
-callback(store(#persist_data{}, term()) -> #persist_data{}).
-callback(values(#persist_data{}) -> term()).

store(Data = #persist_data{module = Module}, Value) ->
    Module:store(Data, Value).

values(Data = #persist_data{module = Module}) ->
    Module:values(Data).