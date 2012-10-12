%% Copyright
-module(epath_tests).
-author("Volodymyr Kyrychenko <volodymyr.kyrychenko@strikead.com>").

-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_stdlib/include/xl_eunit.hrl").

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
    ?assertEquals({ok, {ok, [kernel, stdlib]}},
        epath:select("/$3/[$1 == applications]/$2", ?APP)),
    ?assertEquals({ok, {ok, {vsn, "1"}}},
        epath:select("/$3/[$2 == \"1\"]", ?APP)).

eselect_test() ->
    ?assertEquals({error, error},
        epath:eselect("/$3/[$1 == ~p]/$2", [applic], ?APP, error)).

update_list_test() ->
    L = [{1, a}, {2, b}, {3, {x1, y1}}, {4, d}, {5, {x2, y2}}],
    ?assertEquals({ok, [{1, a}, {2, b}, {3, {z, y1}}, {4, d}, {5, {z, y2}}]},
        epath:update("/[$1 > 2]*/$2/$1", z, L)),
    ?assertEquals({ok, [{1, a}, {2, b}, z, z, z]},
        epath:update("/[$1 > 2]*", z, L)).

update_element_test() ->
    L = [{1, a}, {2, b}, {3, {x1, y1}}, {4, d}, {5, {x2, y2}}],
    ?assertEquals({ok, [{1, a}, {2, b}, {3, {z, y1}}, {4, d}, {5, {x2, y2}}]},
        epath:update("/[$1 > 2]/$2/$1", z, L)),
    ?assertEquals({ok, [{1, a}, {2, b}, z, {4, d}, {5, {x2, y2}}]},
        epath:update("/[$1 > 2]", z, L)).

concat_test() ->
    ?assertEquals({ok, {application, epath, [
        {description, ""},
        {vsn, "1"},
        {registered, []},
        {applications, [epath, kernel, stdlib]},
        {env, []}
    ]}}, epath:concat("/$3/[$1 == applications]/$2", [epath], ?APP)).

select_list_test() ->
    L = [{1, a}, {2, b}, {3, {x1, y1}}, {4, d}, {5, {x2, y2}}],
    ?assertEquals({ok, [x1, x2]}, epath:select("/[$1 > 2]*/$2/$1", L)).
