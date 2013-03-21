%% Copyright
-module(persist_index_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

-record(test, {id, name}).
index_test() ->
    Index = persist_index:new(name, fun(#test{id = Id, name = Names}) -> [{Name, Id, Id} || Name <- Names] end, fun(#test{id = Id}) -> Id end),
    persist_index:index(Index, #test{id = 1, name = [a]}),
    persist_index:index(Index, #test{id = 2, name = [b]}),
    persist_index:index(Index, #test{id = 3, name = [a]}),
    persist_index:index(Index, #test{id = 4, name = [a, c, d]}),
    ?assertEqual([{a, 1, 1}, {a, 3, 3}, {a, 4, 4}], persist_index:lookup(Index, a)),
    ?assertEqual([{b, 2, 2}], persist_index:lookup(Index, b)),
    ?assertEqual([{c, 4, 4}], persist_index:lookup(Index, c)),
    ?assertEqual([{d, 4, 4}], persist_index:lookup(Index, d)),
    ?assertEqual([], persist_index:lookup(Index, x)),
    persist_index:update(Index, #test{id = 4, name = [e, d, f]}),
    ?assertEqual([{a, 1, 1}, {a, 3, 3}], persist_index:lookup(Index, a)),
    ?assertEqual([{d, 4, 4}], persist_index:lookup(Index, d)),
    ?assertEqual([{e, 4, 4}], persist_index:lookup(Index, e)),
    ?assertEqual([{f, 4, 4}], persist_index:lookup(Index, f)).

