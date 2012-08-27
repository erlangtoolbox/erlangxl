%% Copyright
-module(epath_tests).
-author("volodymyr.kyrychenko@strikead.com").

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
%    ?assertEqual({ok, [kernel, stdlib]}, epath:select("/$3/[$3 = applications]/$2", ?APP)).
    ok.
