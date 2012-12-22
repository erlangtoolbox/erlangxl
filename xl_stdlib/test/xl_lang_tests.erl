%% Copyright
-module(xl_lang_tests).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-record(r, {a, b, c}).

record_to_proplist_test() ->
    R = #r{a = 1, b = 2, c = c},
    ?assertEqual([{a, 1}, {b, 2}, {c, c}], xl_lang:record_to_proplist(R, record_info(fields, r))).
