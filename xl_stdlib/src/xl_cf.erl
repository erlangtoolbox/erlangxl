%% Copyright
-module(xl_cf).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([parse_transform/2, flatmap/2, map/2]).

parse_transform(Forms, Options) -> xl_cf_pt:parse_transform(Forms, Options).

flatmap(Fun, List) when is_list(List) -> lists:flatmap(Fun, List);
flatmap(Fun, Data = {xl_stream, _}) ->
    xl_stream:flatmap(Fun, Data).

map(Fun, List) when is_list(List) -> lists:map(Fun, List);
map(Fun, Stream = {xl_stream, _}) -> xl_stream:map(Fun, Stream);
%gb_tree
map(_Fun, T = {0, nil}) -> T;
map(Fun, T = {X, {_, _, _, _}}) when is_integer(X) -> gb_trees:map(fun(K, V) -> Fun({K, V}) end, T).
