-module(strikead_escript_tests).

-include_lib("eunit/include/eunit.hrl").

read_file_test() ->
    ok = strikead_file:delete("/tmp/test"),
    {ok, {_, Zip}} = zip:create("mem", [{"x.txt", <<"xxx">>}, {"a/b/c.txt", <<"cccc">>}], [memory]),
    {ok, EScript} = escript:create(binary, [
        {shebang, default},
        {comment, default},
        {emu_args, undefined},
        {archive, Zip}
    ]),
    File = "/tmp/test/testescript",
    ok = strikead_file:write_file(File, EScript),
    ?assertEqual({ok, <<"xxx">>}, strikead_escript:read_file(File, "x.txt")).
