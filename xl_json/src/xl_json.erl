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
-module(xl_json).

-compile({parse_transform, do}).
-behaviour(xl_json_api).

-export([to_json/1, from_json/1, to_abstract/1, get_value/2, bind/3, field_name_presentation/1, parse_file/1]).

-define(BACKEND_JSON_API, xl_json_jiffy).

-spec(to_json(xl_json_api:json_document()) -> binary()).
to_json(<<>>) -> "\"\"";
to_json(X) when is_binary(X) -> xl_string:format("~p", [binary_to_list(X)]);
to_json(X) when is_number(X) -> xl_string:format("~p", [X]);
to_json(true) -> "true";
to_json(false) -> "false";
to_json(undefined) -> "null";
to_json(X) when is_atom(X) -> xl_string:format("\"~s\"", [X]);
to_json({ok, X}) -> to_json(X);
to_json(X = [H | _]) when is_tuple(H) andalso size(H) == 2 ->
    "{" ++ string:join([xl_string:format("\"~s\":", [K]) ++ to_json(V)
        || {K, V} <- X], ",") ++ "}";
to_json(X) when is_list(X) -> "[" ++ string:join([to_json(E) || E <- X], ",") ++ "]".

-spec(from_json(iolist()) -> error_m:monad(xl_json_api:json_document())).
from_json(Source) ->
    case ?BACKEND_JSON_API:from_json(Source) of
        {ok, Doc} -> {ok, ?BACKEND_JSON_API:to_abstract(Doc)};
        E -> E
    end.

-spec(get_value(atom(), xl_json_api:json_document()) -> option_m:monad(any())).
get_value(Field, Doc) -> xl_lists:kvfind(Field, Doc).

-spec(bind(fun((atom(), term(), tuple()) -> tuple()), tuple(), xl_json_api:json_document()) -> tuple()).
bind(Callback, Seed, Doc) ->
    lists:foldl(fun({Name, Value}, T) -> Callback(Name, Value, T) end, Seed, Doc).


-spec(to_abstract(xl_json_api:json_document()) -> xl_json_api:abstract_json_document()).
to_abstract(Doc) -> Doc.

-spec(field_name_presentation(atom()) -> term()).
field_name_presentation(Name) -> Name.

-spec(parse_file(file:name()) -> error_m:monad(xl_json_api:json_document())).
parse_file(Path) ->
    do([error_m ||
        Data <- xl_file:read(Path),
        from_json(Data)
    ]).
