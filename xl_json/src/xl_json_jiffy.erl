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
-module(xl_json_jiffy).
-author("volodymyr.kyrychenko@strikead.com").

-behaviour(xl_json_api).

%% API
-export([from_json/1, get_value/2, to_abstract/1, to_json/1, bind/3, field_name_presentation/1]).

-spec(from_json(iolist()) -> error_m:monad(xl_json_api:json_document())).
from_json(Source) ->
    try
        {ok, jiffy:decode(Source)}
    catch
        _:E -> {error, E}
    end.

-spec(get_value(atom(), xl_json_api:json_document()) -> option_m:monad(any())).
get_value(Field, {Fields}) -> xl_lists:kvfind(atom_to_binary(Field, utf8), Fields).

-spec(bind(fun((atom(), term(), tuple()) -> tuple()), tuple(), xl_json_api:json_document()) -> tuple()).
bind(Callback, Seed, {Fields}) ->
    lists:foldl(fun({Name, Value}, T) -> Callback(Name, Value, T) end, Seed, Fields).

-spec(to_abstract(xl_json_api:json_document()) -> xl_json_api:abstract_json_document()).
to_abstract({Fields}) -> [{xl_convert:to(atom, K), to_abstract(V)} || {K, V} <- Fields];
to_abstract(List) when is_list(List) -> [to_abstract(V) || V <- List];
to_abstract(null) -> undefined;
to_abstract(X) -> X.

-spec(to_json(xl_json_api:json_document()) -> binary()).
to_json(Doc) -> jiffy:encode(Doc).

-spec(field_name_presentation(atom()) -> term()).
field_name_presentation(Name) -> xl_convert:to(binary, Name).
