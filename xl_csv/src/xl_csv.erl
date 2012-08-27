-module(xl_csv).

-export([parse_line/1, lines/1, parse_file/1]).

parse_line("") -> [];
parse_line(L) ->
    {Item, Rest} = parse_item_start(xl_string:strip(L)),
    [Item | parse_line(Rest)].

parse_item_start([$" | T]) -> parse_item("", T);
parse_item_start([$, | T]) -> parse_item_start(xl_string:strip(T)).

parse_item(Acc, [$"]) -> {lists:reverse(Acc), []};
parse_item(Acc, [$", $" | T]) -> parse_item([$" | Acc], T);
parse_item(Acc, [$", _ | T]) -> {lists:reverse(Acc), T};
parse_item(Acc, [C | T]) -> parse_item([C | Acc], T).


lines(S) -> xl_stream:map(fun(L) -> parse_line(L) end, S).

-spec parse_file/1 :: (file:name()) -> error_m:monad([[string()]]).
parse_file(Path) ->
    xl_file:using(Path, [read], fun(File) ->
        xl_stream:to_list(xl_csv:lines(xl_io:lines(File)))
    end).
