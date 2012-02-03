-module(strikead_vmfile_tests).

-include_lib("eunit/include/eunit.hrl").

pread_test() ->
    {ok, _} = strikead_auto:using(strikead_vmfile, [strikead_eunit:resource(?MODULE, "vmfile.txt"), [{segment, 32}]], fun(File) ->
        {ok, <<"1234567890">>} = file:pread(File, 0, 10),
        {ok, <<"6789012345">>} = file:pread(File, 5, 10),
        {ok, <<"6781\n22345">>} = file:pread(File, 45, 10),
        {ok, <<"67860">>} = file:pread(File, 295, 10),
        {ok, <<>>} = file:pread(File, 295, 0),
        eof = file:pread(File, 395, 10),
        eof = file:pread(File, 395, 0),
        eof = file:pread(File, 300, 0),
        eof = file:pread(File, 300, 1),
        {ok, <<"0">>} = file:pread(File, 299, 1),
        {ok, <<>>} = file:pread(File, 299, 0)
    end).

lines_test() ->
    F = strikead_eunit:resource(?MODULE, "vmfile.txt"),
    Io = strikead_autofile:using(F, [read], fun(File) -> strikead_stream:to_list(strikead_io:parse_lines(File)) end),
    Vm = strikead_auto:using(strikead_vmfile, [F, [{segment, 50}]], fun(File) -> strikead_stream:to_list(strikead_vmfile:parse_lines(File)) end),
    Io = Vm.

