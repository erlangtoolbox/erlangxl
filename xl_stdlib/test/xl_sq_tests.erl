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
-module(xl_sq_tests).
-author("volodymyr.kyrychenko@strikead.com").

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
    ?assertEquals({ok, [kernel, stdlib]},
        xl_sq:select([3, fun({applications, _}) -> true; (_) -> false end, 2], ?APP)),
    ?assertEquals({ok, [kernel, stdlib]},
        xl_sq:select([3, {'==', 1, applications}, 2], ?APP)),
    ?assertEquals({ok, kernel},
        xl_sq:select([3, {'==', 1, applications}, 2, 1], ?APP)),
    ?assertEquals({ok, {vsn, "1"}},
        xl_sq:select([3, {'==', 2, "1"}], ?APP)),
    ?assertEquals({ok, description},
        xl_sq:select([3, {'/=', 2, undefined}, 1], ?APP)),
    ?assertEquals({ok, undefined},
        xl_sq:select([3, {'==', 2, "2"}], ?APP)),
    L = [{1, a}, {2, b}, {3, {x1, y1}}, {4, d}, {5, {x2, y2}}],
    ?assertEquals({error, {structural_mismatch, [1], d}}, xl_sq:select([{all, {'>', 1, 2}}, 2, 1], L)),
    L2 = [{1, a}, {2, b}, {3, {x1, y1}}, {5, {x2, y2}}],
    ?assertEquals({ok, [x1, x2]}, xl_sq:select([{all, {'>', 1, 2}}, 2, 1], L2)).

update_replace_test() ->
    L = [{1, a}, {2, b}, {3, {x1, y1}}, {5, {x2, y2}}],
    ?assertEquals({ok, [{1, a}, {2, b}, {3, {z, y1}}, {5, {z, y2}}]},
        xl_sq:update([{all, {'>', 1, 2}}, 2, 1], {replace, z}, L)),
    ?assertEquals({ok, [{1, a}, {2, b}, z, z]},
        xl_sq:update([{all, {'>', 1, 2}}], {replace, z}, L)),
    ?assertEquals({ok, [{1, a}, {2, b}, {3, {z, y1}}, {5, {x2, y2}}]},
        xl_sq:update([{'>', 1, 2}, 2, 1], {replace, z}, L)),
    ?assertEquals({ok, [{1, a}, {2, b}, z, {5, {x2, y2}}]},
        xl_sq:update([{'>', 1, 2}], {replace, z}, L)).

update_append_test() ->
    ?assertEquals({ok, {application, epath, [
        {description, ""},
        {vsn, "1"},
        {registered, []},
        {applications, [epath, kernel, stdlib]},
        {env, []},
        {'env@1.1.1.1', [a]}
    ]}}, xl_sq:update([3, {'==', 1, applications}, 2], {append, [epath]}, ?APP)).

