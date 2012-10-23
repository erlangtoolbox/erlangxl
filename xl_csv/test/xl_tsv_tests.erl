-module(xl_tsv_tests).

-include_lib("eunit/include/eunit.hrl").

parse_line_test() ->
    ?assertEqual(["12", "23", "34"], xl_tsv:parse_line("12\t23\t34")).

parse_line_empty_test() ->
    ?assertEqual(["", "23", ""], xl_tsv:parse_line("\t23\t")).

parse_file_test() ->
    Path = xl_eunit:resource(?MODULE, "tsv.tsv"),
    ?assertEqual({ok, [
        [<<"key">>, <<"value">>],
        [<<"1">>, <<"one">>],
        [<<"2">>, <<"two">>],
        [<<"3">>, <<"three">>]
    ]}, xl_tsv:parse_file(Path, xl_tsv:comment_filter([$#]))).
