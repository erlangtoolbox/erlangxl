Definitions.

Rules.
\<\<"(\\\^.|\\.|[^"])*"\>\> : {token, {binary, TokenLine, binstr_to_bin(TokenChars)}}.
"(\\\^.|\\.|[^"])*" : {token, {string, TokenLine, xl_string:unquote(TokenChars)}}.
\$[1-9][0-9]* : {token, {element, TokenLine, element_x(TokenChars)}}.
=>|=<|==|>|< : {token, {cmp, TokenLine, list_to_atom(TokenChars)}}.
\[|\]|\(|\)|\,|; : {token, {list_to_atom(TokenChars), TokenLine}}.
[a_z][a-zA-Z0-9\_]* : {token, {atom, TokenLine, TokenChars}}.
'[^']+' : {token, {atom, TokenLine, TokenChars}}.
/ : skip_token.
\s+ : skip_token.

Erlang code.
-export([parse/1]).

parse(Str) ->
    R = string(Str),
    {element(1, R), element(2, R)}.

element_x([_ | X]) -> list_to_integer(X).

binstr_to_bin(X) ->
    list_to_binary(xl_string:unquote(string:substr(X, 3, string:len(X)-4))).

