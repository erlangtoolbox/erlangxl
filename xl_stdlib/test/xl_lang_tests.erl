%% Copyright
-module(xl_lang_tests).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-include_lib("eunit/include/eunit.hrl").
-include("xl_lang.hrl").

-record(r, {a, b, c}).

record_to_proplist_test() ->
    R = #r{a = 1, b = 2, c = c},
    ?assertEqual([{a, 1}, {b, 2}, {c, c}], ?RECORD_TO_PROPLIST(R, r)).

safe_call_fail_test() ->
    ?assertEqual(result, xl_lang:safe_call(fun() -> error(fail) end, result)).

safe_call_fail_with_fun_test() ->
    ?assertEqual(result, xl_lang:safe_call(fun() -> error(fail) end, fun() -> result end)).

safe_call_success_test() ->
    ?assertEqual(ok, xl_lang:safe_call(fun() -> ok end, result)).
