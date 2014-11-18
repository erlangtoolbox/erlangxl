%% =============================================================================
%%  The MIT License (MIT)
%%
%%  Copyright (c) 2014 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy of
%%  this software and associated documentation files (the "Software"), to deal in
%%  the Software without restriction, including without limitation the rights to
%%  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
%%  the Software, and to permit persons to whom the Software is furnished to do so,
%%  subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in all
%%  copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
%%  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
%%  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
%%  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%%  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%% =============================================================================
%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2014 2:45 AM
%%%-------------------------------------------------------------------
-module(xl_code_tests).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

escape_test() ->
    {ok, [{prefix_xl_code_testapp, Version}]} =
        xl_code:escape_applications(prefix_, ["../out/production/xl_code_testapp*"], "/tmp/escaped", [testkeep]),
    code:add_patha("/tmp/escaped/prefix_xl_code_testapp-" ++ Version ++ "/ebin"),
    ?assertEqual({ok, true}, xl_file:exists("/tmp/escaped/prefix_xl_code_testapp-" ++ Version ++ "/priv/test.txt")),
    ?assertEqual(world, prefix_testapp:hello()),
    ?assertEqual(world, testkeep:hello()).
