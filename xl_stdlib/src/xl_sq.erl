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
-module(xl_sq).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

%% API
-export([select/2, update/3]).

-export_type([path/0]).

-type(path() :: [pos_integer() | fun((term()) -> boolean()) | {comparison(), pos_integer(), term()}]).
-type(comparison() :: '==' | '/=' | '=>' | '<=' | '=:=' | '=:=' | '<' | '>').
-type(update() :: {replace, term()} | {append, [term()]} | delete).

-define(is_correct_tuple(N, T), (is_tuple(Target) andalso N =< size(Target))).
-define(is_correct_list(N, T), (is_list(Target) andalso N =< length(Target))).
-define(is_comparison(Op), (Op == '==' orelse Op == '/=' orelse Op == '=>' orelse Op == '<='
    orelse Op == '=:=' orelse Op == '=:=' orelse Op == '<' orelse Op == '>')).
-define(is_match_predicate(P), ((is_tuple(P) andalso size(P) == 3 andalso ?is_comparison(element(1, P))) orelse is_function(P, 1))).

-spec(select([path()], term()) -> error_m:monad(term())).
select([], Target) -> {ok, Target};
select(_Path, undefined) -> {ok, undefined};
select([N | Path], Target) when is_integer(N) andalso ?is_correct_tuple(N, Target) ->
    select(Path, element(N, Target));
select([N | Path], Target) when is_integer(N) andalso ?is_correct_list(N, Target) ->
    select(Path, lists:nth(N, Target));
select([P | Path], Target) when is_list(Target) andalso (?is_match_predicate(P) orelse is_function(P, 1)) ->
    case xl_lists:efind(predicate(P), Target) of
        {ok, undefined} -> {ok, undefined};
        {ok, {ok, V}} -> select(Path, V);
        E -> E
    end;
select([{all, P} | Path], Target) when is_list(Target) andalso (?is_match_predicate(P) orelse is_function(P, 1)) ->
    do([error_m ||
        Targets <- xl_lists:efilter(predicate(P), Target),
        xl_lists:emap(fun(T) -> select(Path, T) end, Targets)
    ]);
select(Path, Target) -> {error, {structural_mismatch, Path, Target}}.

predicate(F) when is_function(F) -> fun(X) -> {ok, F(X)} end;
predicate({Op, N, Value}) when is_integer(N) -> predicate({Op, [N], Value});
predicate({Op, Path, Value}) when is_list(Path) ->
    fun(Target) ->
        case select(Path, Target) of
            {ok, V} -> {ok, erlang:Op(V, Value)};
            E -> E
        end
    end.

-spec(update([path()], update(), term()) -> error_m:monad(term())).
update([], _Update, undefined) -> {ok, undefined};
update([], {replace, Value}, _Target) -> {ok, Value};
update([], {append, List}, Target) when is_list(Target) andalso is_list(List) ->
    {ok, lists:append([Target, List])};
update([], {append, List}, Target) when is_tuple(Target) andalso is_list(List) ->
    {ok, list_to_tuple(lists:append([tuple_to_list(Target), List]))};
update([], F, Target) when is_function(F, 1) -> {ok, F(Target)};
update([N], delete, Target) when is_integer(N) andalso ?is_correct_tuple(N, Target) ->
    {H, [_ | T]} = lists:split(N - 1, tuple_to_list(Target)),
    {ok, list_to_tuple(H ++ T)};
update([N | Path], Update, Target) when is_integer(N) andalso ?is_correct_tuple(N, Target) ->
    Element = element(N, Target),
    case update(Path, Update, Element) of
        {ok, X} -> {ok, setelement(N, Target, X)};
        E -> E
    end;
update([P | Path], Update, Target) when is_list(Target) andalso ?is_match_predicate(P) ->
    Predicate = xl_lists:not_epredicate(predicate(P)),
    case xl_lists:esplitwith(Predicate, Target) of
        {ok, {Head, [T | Tail]}} ->
            case update(Path, Update, T) of
                {ok, X} -> {ok, Head ++ [X | Tail]};
                E -> E
            end;
        {ok, {Head, []}} -> {ok, Head};
        E -> E
    end;
update([{all, P} | Path], Update, Target) when is_list(Target) andalso ?is_match_predicate(P) ->
    Predicate = predicate(P),
    xl_lists:emap(fun(T) ->
        case Predicate(T) of
            {ok, true} -> update(Path, Update, T);
            {ok, false} -> {ok, T};
            E -> E
        end
    end, Target);
update(Path, Update, Target) -> {error, {structural_mismatch, Path, Update, Target}}.
