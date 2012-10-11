%% Copyright
-module(xl_uid_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

next_test() ->
    ?assert(xl_uid:next() < xl_uid:next()).
