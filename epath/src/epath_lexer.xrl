Definitions.

Rules.
\$[1-9][0-9]* : {token, {element, TokenLine, element_x(TokenChars)}}.
\=\>|\=\<|\=|\>|\< : {token, {cmp, TokenLine, list_to_atom(TokenChars)}}.
\[|\]|\(|\)|\,|; : {token, {list_to_atom(TokenChars), TokenLine}}.
[a_z][a-zA-Z0-9\_]* : {token, {atom, TokenLine, TokenChars}}.
'[^']+' : {token, {atom, TokenLine, TokenChars}}.
/ : skip_token.
\s+ : skip_token.

Erlang code.
element_x([_ | X]) -> list_to_integer(X).
