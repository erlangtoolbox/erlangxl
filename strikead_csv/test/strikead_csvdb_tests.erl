-module(strikead_csvdb_tests).

-include_lib("eunit/include/eunit.hrl").
-include("strikead_csvdb.hrl").

-behaviour(strikead_csvdb_handler).
-export([extract/1, find/2, format/2]).

index_test() ->
    Csv = strikead_eunit:resource(?MODULE, "csvdb.csv"),
    os:cmd("rm " ++ strikead_csvdb:cache_path(Csv)),
    Header = ["key", "value"],
    Tree = {3, {1, {entry, "one", 14, 10}, nil, {2, {entry, "two", 24, 10}, nil, {3, {entry, "three", 34, 12}, nil, nil}}}},
    {Header, Tree, File} = strikead_csvdb:start_index(Csv, fun([Key, Value]) -> {list_to_integer(Key), Value} end),
    file:close(File).

lookup_test() ->
    Csv = strikead_eunit:resource(?MODULE, "csvdbbig.csv"),
    os:cmd("rm " ++ strikead_csvdb:cache_path(Csv)),
    strikead_csvdb:start_link(Csv, ?MODULE, sync),
    try
        ?assertEqual({"1", "one"}, strikead_csvdb:lookup(1)),
        ?assertEqual({"3", "three"}, strikead_csvdb:lookup(4)),
        ?assertEqual({"33", "three"}, strikead_csvdb:lookup(33)),
        ?assertEqual(not_found, strikead_csvdb:lookup(23))
    after
        strikead_csvdb:stop()
    end.

unload_load_test() ->
    Csv = strikead_eunit:resource(?MODULE, "csvdbbig.csv"),
    os:cmd("rm " ++ strikead_csvdb:cache_path(Csv)),
    strikead_csvdb:start_link(Csv, ?MODULE, sync),
    try
        strikead_csvdb:unload(),
        ?assertEqual(not_available, strikead_csvdb:lookup(1)),
        strikead_csvdb:load(Csv, ?MODULE),
        timer:sleep(100),
        ?assertEqual({"1", "one"}, strikead_csvdb:lookup(1))
    after
        strikead_csvdb:stop()
    end.


extract([Key, Value]) -> {list_to_integer(Key), Value}.
accept_if_possible(Key, Value = #entry{value = "three"}) -> {Key, Value};
accept_if_possible(_Key, _Value) -> not_found.

find(_, nil) -> not_found;
find(Key, {Key, V, _, _}) -> {Key, V};
find(Key, {K, V, _, nil}) when Key > K -> accept_if_possible(K, V);
find(Key, {K, V, nil, _}) when Key < K -> accept_if_possible(K, V);
find(Key, {K, V, _, {KR, _, _, _}}) when Key > K, Key < KR -> accept_if_possible(K, V);
find(Key, {K, _, _, Right}) when Key > K -> find(Key, Right);
find(Key, {K, _, Left, _}) when Key < K -> find(Key, Left).

format(_Header, [Key, Value]) -> {Key, Value}.
