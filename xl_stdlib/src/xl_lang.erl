-module(xl_lang).

-export([ifelse/3, record_to_proplist/2]).

ifelse(true, Then, _) -> Then;
ifelse(false, _, Else) -> Else.

record_to_proplist(Record, Fields) ->
    xl_lists:imap(fun(Field, I) -> {Field, element(I + 1, Record)} end, Fields).