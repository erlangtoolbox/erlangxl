-module(strikead_lang_tests).

-include_lib("eunit/include/eunit.hrl").

do_test() ->
    ?assertEqual({right, ok}, strikead_lang_do:do_test(ok)),
    ?assertEqual({left, first_failed}, strikead_lang_do:do_test(notok)),
    ?assertEqual({right, okvalue}, strikead_lang_do:ok_test(ok)),
    ?assertEqual({left, verybad}, strikead_lang_do:ok_test(nok)).

to_left_test() ->
    ?assertEqual({right, ok}, strikead_lang:to_left(ok, left)),
    ?assertEqual({left, left}, strikead_lang:to_left(badarg, left)),
    ?assertEqual({right, ok}, strikead_lang:to_left({right,ok}, left)),
    ?assertEqual({left, left}, strikead_lang:to_left({left, notok}, left)).
