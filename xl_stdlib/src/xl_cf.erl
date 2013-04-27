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
-module(xl_cf).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([parse_transform/2, flatmap/2, map/2]).

parse_transform(Forms, Options) -> xl_cf_pt:parse_transform(Forms, Options).

flatmap(Fun, List) when is_list(List) -> lists:flatmap(Fun, List);
flatmap(Fun, Data = {xl_stream, _}) -> xl_stream:flatmap(Fun, Data).

-spec(map(fun((term())-> term()), [term()] | xl_stream:stream(A) | gb_tree()) ->
    [term()] | xl_stream:stream(A) | gb_tree()).
map(Fun, List) when is_list(List) -> lists:map(Fun, List);
map(Fun, Stream = {xl_stream, _}) -> xl_stream:map(Fun, Stream);
%gb_tree
map(_Fun, T = {0, nil}) -> T;
map(Fun, T = {X, {_, _, _, _}}) when is_integer(X) -> gb_trees:map(fun(K, V) -> Fun({K, V}) end, T).
