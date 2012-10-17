%% Copyright
-module(xl_tsv).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([parse_line/1, lines/1, parse_file/1, parse_file/2, comment_filter/1]).

parse_line(L) when is_binary(L) -> binary:split(binary:replace(L, [<<"\n">>, <<"\r">>], <<>>), <<"\t">>, [global]);
parse_line(L) when is_list(L) -> xl_lists:split_by(L, $\t).

-spec lines/1 :: (xl_stream:stream()) -> xl_stream:stream().
lines(S) -> xl_stream:map(fun(L) -> parse_line(L) end, S).

-spec parse_file/1 :: (file:name()) -> error_m:monad([[string()]]).
parse_file(Path) -> parse_file(Path, fun(_) -> true end).

-spec parse_file/2 :: (file:name(), fun((binary()) -> boolean())) -> error_m:monad([[string()]]).
parse_file(Path, LineFilter) ->
    xl_file:using(Path, [read], fun(File) ->
        xl_stream:to_list(lines(xl_stream:filter(LineFilter, xl_io:lines(File))))
    end).

-spec comment_filter/1 :: ([char()]) -> fun((binary()) -> boolean()).
comment_filter(Chars) ->
    fun(L) ->
        case binary:replace(L, [<<" ">>, <<"\t">>, <<"\n">>, <<"\r">>], <<"">>, [global]) of
            <<>> -> false;
            <<X, _Rest/binary>> -> not lists:member(X, Chars)
        end
    end.