%% Copyright
-module(xl_uid).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

%% API
-export([next/0, next_hex/0, start/0]).

next() -> {ok, [P]} = xl_state:get(?MODULE, prefix),
    P + xl_calendar:now_micros().

next_hex() ->
    integer_to_list(next(), 16).

start() ->
    xl_state:new(?MODULE, [{read_concurrency, true}]),
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
                    xl_state:set(?MODULE, prefix, P bsl 64);
                undefined ->
                    xl_state:set(?MODULE, prefix, 0)
            end;
        _ ->
            xl_state:set(?MODULE, prefix, 0)
    end.