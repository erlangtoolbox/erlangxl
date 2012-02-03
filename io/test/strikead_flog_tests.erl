-module(strikead_flog_tests).

-include_lib("eunit/include/eunit.hrl").

tsv_fout_test() ->
    os:cmd("rm -r /tmp/test"),
    strikead_flog:start_link(tsv, "/tmp/test", fun strikead_flog:format_tsv/2),
    strikead_flog:log(tsv, [a,"b",1]),
    strikead_flog:log(tsv, [a,b,1]),
    strikead_flog:log(tsv, [a,b,c]),
    ?assertEqual({ok, <<"a\tb\t1\na\tb\t1\na\tb\tc\n">>}, file:read_file(find_file())).

tsv_terms_fout_test() ->
    os:cmd("rm -r /tmp/test"),
    strikead_flog:start_link(tsv_terms, "/tmp/test", fun strikead_flog:format_tsv/2),
    strikead_flog:log(tsv_terms, [a,[{1,"1"}],1]),
    ?assertEqual({ok, <<"a\t[{1,\"1\"}]\t1\n">>}, file:read_file(find_file())).

terms_fout_test() ->
    os:cmd("rm -r /tmp/test"),
    strikead_flog:start_link(terms, "/tmp/test", fun strikead_flog:format_terms/2),
    strikead_flog:log(terms, [aaaaaaaaaaaaaaaaaaaaaaaaaaa,bbbbbbbbbbbbbbbbbbbbbbbb,ccccccccccccccccccccc,dddddddddddddddddddddddddd]),
    strikead_flog:log(terms, [a,b,c]),
    strikead_flog:log(terms, [a,b,c]),
    {ok, R} = file:read_file(find_file()),
    ?assertEqual(<<"[aaaaaaaaaaaaaaaaaaaaaaaaaaa,bbbbbbbbbbbbbbbbbbbbbbbb,ccccccccccccccccccccc,dddddddddddddddddddddddddd]\n[a,b,c]\n[a,b,c]\n">>, R).

find_file() ->
    {ok, [D]} = file:list_dir("/tmp/test/"),
    {ok, [F]} = file:list_dir("/tmp/test/" ++ D),
    "/tmp/test/" ++ D ++ "/" ++ F.
