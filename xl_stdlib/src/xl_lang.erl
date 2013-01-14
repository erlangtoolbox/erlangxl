-module(xl_lang).

-export([ifelse/3, record_to_proplist/2, safe_call/2]).

ifelse(true, Then, _) -> result(Then);
ifelse(false, _, Else) -> result(Else).

record_to_proplist(Record, Fields) ->
    xl_lists:imap(fun(Field, I) -> {Field, element(I + 1, Record)} end, Fields).

safe_call(F, R) when is_function(F, 0) ->
    try 
        F()
    catch
        _:_ ->
            result(R)
    end.

 result(R) when is_function(R, 0) ->
 	R();
 result(R) ->
 	R.
