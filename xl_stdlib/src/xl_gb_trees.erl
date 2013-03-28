%% Copyright
-module(xl_gb_trees).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([lookup/2]).

lookup(Key, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        none -> undefined;
        {value, V} -> {ok, V}
    end.
