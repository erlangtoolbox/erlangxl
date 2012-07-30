-module(strikead_lang_do).

-compile({parse_transform, strikead_lang}).

-export([do_test/1, ok_test/1]).

do_test(Value) ->
   strikead_lang:do([f(Y) || X <- lr(Value, first_failed), Y <- lr(X, second_failed)]).

lr(ok, _) -> {right, ok};
lr(_, Msg) -> {left, Msg}.

f(Y) -> Y.

okf(ok) -> {ok, okvalue};
okf(_) -> verybad.


ok_test(Value) ->
    strikead_lang:do([X || X <- okf(Value)]).

% Local Variables:
% indent-tabs-mode: nil
% End:
