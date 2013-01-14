%% Copyright
-module(xl_lang_tests).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-define(SUCCESSFUN, fun() -> lists:nth(1, [ok]) end).
-define(FAILFUN, fun() -> lists:nth(1, not_a_list) end).
-define(FUNRES, fun() -> safe end).
-define(RES, safe).
-define(SUCCESSRES, ok).

-record(r, {a, b, c}).

record_to_proplist_test() ->
    R = #r{a = 1, b = 2, c = c},
    ?assertEqual([{a, 1}, {b, 2}, {c, c}], xl_lang:record_to_proplist(R, record_info(fields, r))).

safe_call_fail_test() ->
	?assertEqual(?RES, xl_lang:safe_call(?FAILFUN, ?RES)).

safe_call_fail_with_fun_test() ->
	?assertEqual(?RES, xl_lang:safe_call(?FAILFUN, ?FUNRES)).

safe_call_success_test() ->
	?assertEqual(?SUCCESSRES, xl_lang:safe_call(?SUCCESSFUN, ?RES)).
