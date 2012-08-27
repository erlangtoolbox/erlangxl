-module(xl_escript_tests).

-include_lib("eunit/include/eunit.hrl").

read_file_test() ->
    ok = xl_file:delete("/tmp/test"),
    {ok, {_, Zip}} = zip:create("mem", [{"x.txt", <<"xxx">>}, {"a/b/c.txt", <<"cccc">>}], [memory]),
    {ok, EScript} = escript:create(binary, [
        {shebang, default},
        {comment, default},
        {emu_args, undefined},
        {archive, Zip}
    ]),
    File = "/tmp/test/testescript",
    ok = xl_file:write_file(File, EScript),
    ?assertEqual({ok, <<"xxx">>}, xl_escript:read_file(File, "x.txt")).
