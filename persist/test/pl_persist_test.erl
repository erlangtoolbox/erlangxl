%% Copyright
-module(pl_persist_test).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

-record(testobj, {
    id,
    name
}).

persist_test() ->
    {ok, P} = open(),
    T1 = #testobj{id = "1", name = "n1"},
    T2 = #testobj{id = "2", name = "n2"},
    ?assertEqual(ok, pl_persist:mutate(P, fun(Data) ->
        NewState = pl_persist_data:store(Data, [T1, T2]),
        {NewState}
    end)),
    ?assertEqual([T1, T2], pl_persist:read(P, fun(Data) ->
        pl_persist_data:values(Data)
    end)),
    ?assertEqual([T1, T2], pl_persist:read(P, fun(Data) ->
        pl_persist_data:values(Data)
    end)).
%%     ?assertEqual([T1, T2], persist:select(P)),
%%     ?assertEqual({ok, T1}, persist:get(P, "1")),
%%     ?assertEqual({ok, T2}, persist:get(P, "2")),
%%     ?assertEqual(undefined, persist:get(P, "3")),
%%
%%     persist:delete(P, "1"),
%%     ?assertEqual([T2], persist:select(P)),
%%     ?assertEqual(undefined, persist:get(P, "1")),
%%     ?assertEqual({ok, T2}, persist:get(P, "2")),
%%
%%     ?assertEqual(ok, persist:close(P)).

open() ->
    pl_persist:open(persist:by_index(#testobj.id), [
        {data_backend, pl_persist_data_orddict}
    ]).
