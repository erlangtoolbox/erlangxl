%% Copyright
-module(xl_ets).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([foreach/2, lookup_object/2, cursor/1, cursor/2]).

-spec(foreach(fun((term(), term()) -> ok), ets:tab()) -> ok).
foreach(F, Tab) -> foreach(F, Tab, ets:first(Tab)).

foreach(_F, _Tab, '$end_of_table') -> ok;
foreach(F, Tab, Key) ->
    F(Key, ets:lookup(Tab, Key)),
    foreach(F, Tab, ets:next(Tab, Key)).

-spec(lookup_object(ets:tab(), term()) -> option_m:monad(term())).
lookup_object(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [O] -> {ok, O};
        _ -> undefined
    end.

-spec(cursor(ets:tab()) -> xl_stream:stream(term())).
cursor(Tab) ->
    xl_stream:stream(ets:first(Tab), fun
        ('$end_of_table') -> empty;
        (Key) ->
            case xl_ets:lookup_object(Tab, Key) of
                {ok, O} -> {O, ets:next(Tab, Key)};
                _ -> empty
            end
    end).

-spec(cursor(ets:tab(), fun((term())-> boolean())) -> xl_stream:stream(term())).
cursor(Tab, F) -> xl_stream:filter(F, cursor(Tab)).
