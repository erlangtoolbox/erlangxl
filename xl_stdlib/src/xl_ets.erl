%% Copyright
-module(xl_ets).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([foreach/2]).

-spec(foreach(fun((term(), term()) -> ok), ets:tab()) -> ok).
foreach(F, Tab) -> foreach(F, Tab, ets:first(Tab)).

foreach(_F, _Tab, '$end_of_table') -> ok;
foreach(F, Tab, Key) ->
    F(Key, ets:lookup(Tab, Key)),
    foreach(F, Tab, ets:next(Tab, Key)).

