%% Copyright
-module(epath_lexer_tests).
-author("Volodymyr Kyrychenko <volodymyr.kyrychenko@strikead.com>").

-include_lib("eunit/include/eunit.hrl").

element_test() ->
    ?assertEqual({ok, [
        {element, 1, 1},
        {element, 1, 2}
    ]}, epath_lexer:parse("/$1/$2")).

condition_test() ->
    ?assertEqual({ok, [
        {element, 1, 1},
        {'[', 1},
        {element, 1, 2},
        {cmp, 1, '=='},
        {atom, 1, app},
        {']', 1}
    ]}, epath_lexer:parse("/$1/[$2 == app]")).

string_test() ->
    ?assertEqual({ok, [
        {'[', 1},
        {element, 1, 2},
        {cmp, 1, '=='},
        {string, 1, "a\"pp"},
        {']', 1}
    ]}, epath_lexer:parse("/[$2 == \"a\\\"pp\"]")).

binary_test() ->
    ?assertEqual({ok, [
        {'[', 1},
        {element, 1, 2},
        {cmp, 1, '=='},
        {binary, 1, <<"app">>},
        {']', 1}
    ]}, epath_lexer:parse("/[$2 == <<\"app\">>]")).
