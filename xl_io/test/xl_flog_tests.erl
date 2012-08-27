-module(xl_flog_tests).

-include_lib("eunit/include/eunit.hrl").

tsv_format_test() ->
    os:cmd("rm -r /tmp/test"),
    xl_flog:start_link(tsv, "/tmp/test", fun xl_flog:format_tsv/2),
    xl_flog:log(tsv, [a, "b", 1]),
    xl_flog:log(tsv, [a, b, 1]),
    xl_flog:log(tsv, [a, b, c]),
    xl_flog:flush(tsv),
    xl_flog:stop(tsv),
    ?assertEqual({ok, <<"a\tb\t1\na\tb\t1\na\tb\tc\n">>}, xl_file:read_file(find_file())).

tsv_format_empty_string_test() ->
    os:cmd("rm -r /tmp/test"),
    xl_flog:start_link(tsv_empty, "/tmp/test", fun xl_flog:format_tsv/2),
    xl_flog:log(tsv_empty, [a, "", 1]),
    xl_flog:flush(tsv_empty),
    xl_flog:stop(tsv_empty),
    ?assertEqual({ok, <<"a\t\t1\n">>}, xl_file:read_file(find_file())).

tsv_terms_format_test() ->
    os:cmd("rm -r /tmp/test"),
    xl_flog:start_link(tsv_terms, "/tmp/test", fun xl_flog:format_tsv/2),
    xl_flog:log(tsv_terms, [a, [{1, "1"}], 1]),
    xl_flog:flush(tsv_terms),
    xl_flog:stop(tsv_terms),
    ?assertEqual({ok, <<"a\t[{1,\"1\"}]\t1\n">>}, xl_file:read_file(find_file())).

terms_long_no_linebreak_test() ->
    os:cmd("rm -r /tmp/test"),
    xl_flog:start_link(terms, "/tmp/test", fun xl_flog:format_terms/2),
    xl_flog:log(terms, [aaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbb, ccccccccccccccccccccc, dddddddddddddddddddddddddd]),
    xl_flog:log(terms, [a, b, c]),
    xl_flog:log(terms, [a, b, c]),
    xl_flog:flush(terms),
    xl_flog:stop(terms),
    {ok, R} = xl_file:read_file(find_file()),
    ?assertEqual(<<"[aaaaaaaaaaaaaaaaaaaaaaaaaaa,bbbbbbbbbbbbbbbbbbbbbbbb,ccccccccccccccccccccc,dddddddddddddddddddddddddd]\n[a,b,c]\n[a,b,c]\n">>, R).

find_file() ->
    {ok, [D]} = xl_file:list_dir("/tmp/test/"),
    {ok, [F]} = xl_file:list_dir("/tmp/test/" ++ D),
    "/tmp/test/" ++ D ++ "/" ++ F.

