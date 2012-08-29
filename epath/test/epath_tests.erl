%% Copyright
-module(epath_tests).
-author("Volodymyr Kyrychenko <volodymyr.kyrychenko@strikead.com>").

-include_lib("eunit/include/eunit.hrl").

-define(APP, {application, epath, [
    {description, ""},
    {vsn, "1"},
    {registered, []},
    {applications, [
        kernel,
        stdlib
    ]},
    {env, []}
]}).

select_test() ->
    ?assertEqual({ok, {ok, [kernel, stdlib]}},
        epath:select("/$3/[$1 == applications]/$2", ?APP)),
    ?assertEqual({ok, {ok, {vsn, "1"}}},
        epath:select("/$3/[$2 == \"1\"]", ?APP)).

update_test() ->
    ?assertEqual({ok, {application, epath, [
        {description, ""},
        {vsn, "1"},
        {registered, []},
        {apps, []},
        {env, []}
    ]}}, epath:update("/$3/[$1 == applications]", {apps, []}, ?APP)),
    ?assertEqual({ok, {application, epath, [
        {description, ""},
        {vsn, "1"},
        {registered, []},
        {applications, x},
        {env, []}
    ]}}, epath:update("/$3/[$1 == applications]/$2", x, ?APP)).

concat_test() ->
    ?assertEqual({ok, {application, epath, [
        {description, ""},
        {vsn, "1"},
        {registered, []},
        {applications, [epath, kernel, stdlib]},
        {env, []}
    ]}}, epath:concat("/$3/[$1 == applications]/$2", [epath], ?APP)).
