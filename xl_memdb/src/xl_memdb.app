%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @copyright (C) 2013, strikead.com
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
{application, xl_memdb, [
    {description, "ErlangXl / ETS Memory DB"},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        xl_stdlib
    ]},
    {env, []}
]}.