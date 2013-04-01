%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(xl_yaws_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("yaws/include/yaws_api.hrl").

get_test() ->
    ?assertEqual({[{a, "b"}], []},
        xl_yaws:params([xl_yaws:get(a)], mkargs())),
    ?assertEqual({[], ["Parameter 'x' must be present"]},
        xl_yaws:params([xl_yaws:get(x)], mkargs())).

opt_test() ->
    Args = mkargs(),
    ?assertEqual({[{b, undefined}], []},
        xl_yaws:params([xl_yaws:opt(b)], Args)),
    Xf = fun(X) -> X ++ "-x" end,
    ?assertEqual({[{b, undefined}], []},
        xl_yaws:params([xl_yaws:opt(b, Xf)], Args)),
    ?assertEqual({[{a, "b-x"}], []},
        xl_yaws:params([xl_yaws:opt(a, Xf)], Args)).

convert_test() ->
    Xf = fun(X) -> X ++ "-x" end,
    ?assertEqual({[{a, "b-x"}], []},
        xl_yaws:params([
            xl_yaws:convert(xl_yaws:opt(a), Xf)], mkargs())).

any_test() ->
    ?assertEqual({[{a, "b"}], []},
        xl_yaws:params([xl_yaws:any([
            xl_yaws:get(x), xl_yaws:opt(a)], "Not found")], mkargs())).

as_test() ->
    ?assertEqual({[{z, "b"}], []},
        xl_yaws:params([xl_yaws:as(z, xl_yaws:any([
            xl_yaws:get(x), xl_yaws:opt(a)], "Not Found"))], mkargs())).

list_test() ->
    ?assertEqual({[{x, ["x", "y"]}], []},
        xl_yaws:params([xl_yaws:list(x)], mkargs([{x, "x"}, {x, "y"}]))),
    ?assertEqual({[{x, ["x"]}], []},
        xl_yaws:params([xl_yaws:list(x)], mkargs([{x, "x"}]))),
    ?assertEqual({[{x, []}], []},
        xl_yaws:params([xl_yaws:list(x)], mkargs())).

parse_params_test() ->
    ?assertEqual([{a, "b"}, {c, "d"}], xl_yaws:parse_params(mkargs())).

params_test() ->
    ?assertEqual({[{a, "b"}, {c, "d"}], []}, xl_yaws:params([
        xl_yaws:get(a),
        xl_yaws:get(c)
    ], mkargs())).

mkargs(Query) ->
    xl_yaws:clear_parse_caches(),
    #arg{
        req = #http_request{method = 'GET', path = "/test", version = "HTTP/1.1"},
        querydata = string:join([atom_to_list(K) ++ "=" ++ V || {K, V} <- Query], "&")
    }.

mkargs() -> mkargs([{a, "b"}, {c, "d"}]).

