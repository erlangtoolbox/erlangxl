%% Copyright
-module(epath_lexer_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    ?assertEqual({ok, [
        {element, 1, 1},
        {element, 1, 2}
    ], 1}, epath_lexer:string("/$1/$2")),
    ?assertEqual({ok, [
        {element, 1, 1},
        {'[', 1},
        {element, 1, 2},
        {cmp, 1, '='},
        {atom, 1, "app"},
        {']', 1}
    ], 1}, epath_lexer:string("/$1/[$2 = app]")).
