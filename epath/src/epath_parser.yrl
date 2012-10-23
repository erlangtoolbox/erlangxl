Nonterminals epath selector arg expression.

Terminals element atom string binary float integer '[' ']' '(' ')' ',' ';' '*' cmp.

Rootsymbol epath.

epath -> selector : ['$1'].
epath -> selector epath : ['$1' | '$2'].
selector -> element : {element, token_value('$1')}.
selector -> '[' expression ']' '*' : {select, '$2'}.
selector -> '[' expression ']' : {find, '$2'}.
expression -> arg cmp arg : {token_value('$2'), '$1', '$3'}.
arg -> element : {element, token_value('$1')}.
arg -> atom : {atom, token_value('$1')}.
arg -> string : {string, token_value('$1')}.
arg -> binary : {binary, token_value('$1')}.
arg -> float : {binary, token_value('$1')}.
arg -> integer : {binary, token_value('$1')}.

Erlang code.
token_value({_, _, X}) -> X;
token_value({X, _}) -> X.

