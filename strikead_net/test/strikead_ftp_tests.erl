-module(strikead_ftp_tests).

% ftp config
%  username: ftpuser
%  password: ftpus3r
%  home: /home/ftpuser
%  files: test/test1.csv == <<"test1, data1, values1\n">>
%         test/test2.csv == <<"test2, data2, values2\n">>

-include_lib("eunit/include/eunit.hrl").
-include_lib("strikead_eunit/include/strikead_eunit.hrl").

setup() ->
    file:make_dir("/tmp/test"),
    strikead_eunit:explode(?MODULE, "ftp-test", "/home/ftpuser").

cleanup(_T) -> os:cmd("rm -rf /tmp/test"), os:cmd("rm -rf /home/ftpuser/ftp-test").

apply_test(Function) ->
    strikead_ftp:using("localhost", "ftpuser", "ftpus3r", Function).

test_nlist_filter() ->
    apply_test(fun(Pid) ->
        ?assertEqual({ok, ["ftp-test/test1.csv","ftp-test/test2.csv"]},
             strikead_ftp:nlist_filter(Pid, "ftp-test",  "*.csv"))
    end).

test_nlist_find() ->
    apply_test(fun(Pid) ->
    ok = strikead_ftp:cd(Pid, "ftp-test"),
        ?assertEqual({ok, "test2.csv"}, strikead_ftp:find(Pid,  "*2.csv"))
    end).

test_download() ->
    apply_test(fun(Pid) ->
    ok = strikead_ftp:cd(Pid, "ftp-test"),
    strikead_file:delete("/tmp/test/test1.csv"),
    strikead_file:delete("/tmp/test/test2.csv"),
     ?assertEqual(ok, strikead_ftp:download(Pid, "/tmp/test", ["test1.csv", "test2.csv"])),
    ?assertEqual({ok, true}, strikead_file:exists("/tmp/test/test1.csv")),
    ?assertEqual({ok, true}, strikead_file:exists("/tmp/test/test2.csv"))
    end).

test_recv() ->
    apply_test(fun(Pid) ->
    strikead_file:delete("/tmp/test/test1.csv"),
    ?assertEqual(ok, strikead_ftp:recv(Pid, "ftp-test/test1.csv", "/tmp/test/test1.csv")),
    ?assertEqual({ok, true}, strikead_file:exists("/tmp/test/test1.csv"))
    end).

test_recv_bin() ->
    apply_test(fun(Pid) ->
    ?assertEqual({ok, <<"test1, data1, values1\n">>}, strikead_ftp:recv_bin(Pid, "ftp-test/test1.csv"))
    end).

ftp_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [
      fun test_nlist_filter/0,
      fun test_nlist_find/0,
      fun test_download/0,
      fun test_recv/0,
      fun test_recv_bin/0
     ]
    }.

