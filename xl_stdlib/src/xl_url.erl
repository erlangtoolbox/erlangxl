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
-module(xl_url).

-export([to_query/1, escape_params/1, escape/1, substitute/2, domain/2]).

-spec to_query/1 :: ([{atom(), term()}]) -> string().
to_query(List) ->
    string:join(
        [atom_to_list(Key) ++ "=" ++ escape(Value) || {Key, Value} <- List],
        "&").

-spec escape_params/1 :: (xl_lists:kvlist_at())
        -> xl_lists:kvlist_at().
escape_params(List) ->
    lists:map(fun({K, V}) -> {K, escape(V)} end, List).

-spec escape/1 :: (term()) -> string().
escape(V) -> edoc_lib:escape_uri(xl_convert:to(string, V)).

-spec substitute/2 :: (string(), xl_lists:kvlist_at()) -> string().
substitute(Url, Map) ->
    xl_string:substitute(Url, escape_params(Map)).

-spec domain(pos_integer(), string()) -> string().
domain(Level, Domain) ->
    string:join(lists:reverse(element(1, xl_lists:split(Level,
        lists:reverse(string:tokens(Domain, "."))))), ".").

