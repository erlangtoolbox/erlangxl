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
-module(epath_tests).
-author("Volodymyr Kyrychenko <volodymyr.kyrychenko@strikead.com>").

-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_stdlib/include/xl_eunit.hrl").

-define(APP, {application, epath, [
    {description, ""},
    {vsn, "1"},
    {registered, []},
    {applications, [
        kernel,
        stdlib
    ]},
    {env, []},
    {'env@1.1.1.1', [a]}
]}).

select_test() ->
    ?assertEquals({ok, {ok, [kernel, stdlib]}},
        epath:select("/$3/[$1 == applications]/$2", ?APP)),
    ?assertEquals({ok, {ok, [a]}},
        epath:select("/$3/[$1 == ~p]/$2", ['env@1.1.1.1'], ?APP)),
    ?assertEquals({ok, {ok, {vsn, "1"}}},
        epath:select("/$3/[$2 == \"1\"]", ?APP)),
    ?assertEquals({ok, {ok, description}},
        epath:select("/$3/[$2 /= undefined]/$1", ?APP)),
    ?assertEquals({ok, undefined},
        epath:select("/$3/[$2 == \"2\"]", ?APP)).

eselect_test() ->
    ?assertEquals({error, error},
        epath:eselect("/$3/[$1 == ~p]/$2", [applic], ?APP, error)).

update_list_test() ->
    L = [{1, a}, {2, b}, {3, {x1, y1}}, {4, d}, {5, {x2, y2}}],
    ?assertEquals({ok, [{1, a}, {2, b}, {3, {z, y1}}, {4, d}, {5, {z, y2}}]},
        epath:update("/[$1 > 2]*/$2/$1", z, L)),
    ?assertEquals({ok, [{1, a}, {2, b}, z, z, z]},
        epath:update("/[$1 > 2]*", z, L)).

update_element_test() ->
    L = [{1, a}, {2, b}, {3, {x1, y1}}, {4, d}, {5, {x2, y2}}],
    ?assertEquals({ok, [{1, a}, {2, b}, {3, {z, y1}}, {4, d}, {5, {x2, y2}}]},
        epath:update("/[$1 > 2]/$2/$1", z, L)),
    ?assertEquals({ok, [{1, a}, {2, b}, z, {4, d}, {5, {x2, y2}}]},
        epath:update("/[$1 > 2]", z, L)).

concat_test() ->
    ?assertEquals({ok, {application, epath, [
        {description, ""},
        {vsn, "1"},
        {registered, []},
        {applications, [epath, kernel, stdlib]},
        {env, []},
        {'env@1.1.1.1', [a]}
    ]}}, epath:concat("/$3/[$1 == applications]/$2", [epath], ?APP)).

select_list_test() ->
    L = [{1, a}, {2, b}, {3, {x1, y1}}, {4, d}, {5, {x2, y2}}],
    ?assertEquals({ok, [x1, x2]}, epath:select("/[$1 > 2]*/$2/$1", L)).
