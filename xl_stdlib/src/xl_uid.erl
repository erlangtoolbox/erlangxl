%% Copyright
-module(xl_uid).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

%% API
-export([next/0]).

next() ->
    try
        next_uid()
    catch
        error:_ ->
            initialize(),
            next_uid()
    end.

next_uid() ->
    [{prefix, P}] = ets:lookup(?MODULE, prefix),
    P + xl_calendar:now_micros().

initialize() ->
    ets:new(?MODULE, [named_table, protected, {read_concurrency, true}]),
    case inet:getifaddrs() of
        {ok, Ifs} ->
            case xl_lists:find(fun({_, Opts}) ->
                case xl_lists:kvfind(hwaddr, Opts) of
                    {ok, HWAddr} -> lists:any(fun(X) -> X /= 0 end, HWAddr);
                    undefined -> false
                end
            end, Ifs) of
                {ok, {_, Opts}} ->
                    {ok, HWAddr} = xl_lists:kvfind(hwaddr, Opts),
                    <<P:48>> = list_to_binary(HWAddr),
                    ets:insert(?MODULE, {prefix, P bsl 64});
                undefined ->
                    ets:insert(?MODULE, {prefix, 0})
            end;
        _ -> ets:insert(?MODULE, {prefix, 0})
    end.
