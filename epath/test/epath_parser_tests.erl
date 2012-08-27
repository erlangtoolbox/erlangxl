%% Copyright
-module(epath_parser_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    {ok, Tokens, _} = epath_lexer:string("/$1/[$2 = app]"),
    ?assertEqual({ok, [
        {element, 1},
        {find, {'=', {element, 2}, {atom, "app"}}}
    ]}, epath_parser:parse(Tokens)).
