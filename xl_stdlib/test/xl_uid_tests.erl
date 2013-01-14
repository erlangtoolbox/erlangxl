%% Copyright
-module(xl_uid_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

-export([gen/2]).

next_test() ->
    ?assert(xl_uid:next() < xl_uid:next()),
    xl_eunit:performance(uid, fun(_) -> xl_uid:next() end, 100000),
    xl_eunit:performance(uid_hex, fun(_) -> xl_uid:next_hex() end, 100000).

uniq_test() ->
    xl_state:new(state1),
    lists:foreach(fun(X) ->
        spawn(?MODULE, gen, [state1, xl_convert:make_atom([result, X])])
    end, lists:seq(1, 100)),
    timer:sleep(1000),
    [H | T] = xl_state:keys(state1),
    L = lists:foldl(fun(Current, L) -> lists:subtract(L, hd(element(2, xl_state:get(state1, Current)))) end,
        hd(element(2, xl_state:get(state1, H))), T),
    ?assertEqual(1000, length(L)).

gen(State, Name) ->
    L = [xl_uid:next() || _ <- lists:seq(1, 1000)],
    xl_state:set(State, Name, L).
