%% Copyright
-module(xl_uid_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

next_test() ->
    ?assert(xl_uid:next() < xl_uid:next()),
    xl_eunit:performance(uid, fun(_) -> xl_uid:next() end, 100000),
    xl_eunit:performance(uid_hex, fun(_) -> xl_uid:next_hex() end, 100000).
