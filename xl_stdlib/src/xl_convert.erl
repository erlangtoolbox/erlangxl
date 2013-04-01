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
-module(xl_convert).
-author("volodymyr.kyrychenko@strikead.com").

-type primitive_type() :: atom() | binary() | string() | float() | integer().

%% API
-export([to_string/1, to_atom/1, to_float/1, to_binary/1, to_integer/1,
    make_atom/1, to/2]).

-spec to_string/1 :: (primitive_type()) -> string().
to_string(X) -> to(string, X).

-spec to_atom/1 :: (binary() | string() | atom()) -> atom().
to_atom(X) -> to(atom, X).

-spec to_float/1 :: (string() | binary()) -> float().
to_float(X) -> to(float, X).

-spec to_binary/1 :: (primitive_type()) -> binary().
to_binary(X) -> to(binary, X).

-spec to_integer/1 :: (string() | atom() | binary()) -> integer().
to_integer(X) -> to(integer, X).

-spec make_atom/1 :: ([primitive_type()]) -> atom().
make_atom(L) -> list_to_atom(string:join([to_string(X) || X <- L], "")).

-spec to/2 :: (atom(), term()) -> term().
to(binary, X) when is_binary(X) -> X;
to(binary, X) when is_float(X) -> to(binary, io_lib:format("~p", [X]));
to(binary, X) when is_integer(X) -> to(binary, integer_to_list(X));
to(binary, X) when is_atom(X) -> atom_to_binary(X, utf8);
to(binary, X) when is_list(X) -> list_to_binary(X);

to(string, X) when is_binary(X) -> binary_to_list(X);
to(string, X) when is_atom(X) -> atom_to_list(X);
to(string, X) when is_list(X) -> X;
to(string, X) -> lists:flatten(io_lib:format("~p", [X]));

to(atom, X) when is_binary(X) -> binary_to_atom(X, utf8);
to(atom, X) when is_list(X) -> list_to_atom(X);
to(atom, X) when is_atom(X) -> X;

to(float, X) when is_float(X) -> X;
to(float, X) when is_integer(X) -> X + 0.0;
to(float, X) when is_binary(X) -> to(float, binary_to_list(X));
to(float, X) when is_list(X) ->
    try
        list_to_float(X)
    catch
        _:_ -> float(list_to_integer(X))
    end;


to(integer, X) when is_list(X) -> list_to_integer(X);
to(integer, X) when is_atom(X) -> list_to_integer(atom_to_list(X));
to(integer, X) when is_binary(X) -> list_to_integer(binary_to_list(X)).
