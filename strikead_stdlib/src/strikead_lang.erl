-module(strikead_lang).

-export([ifelse/3, parse_transform/2, to_left/2]).

ifelse(true, Then, _) -> Then;
ifelse(false, _, Else) -> Else.

to_left({right,_}=R, _Left) -> R;
to_left({left,_}, Left) -> {left, Left};
to_left(X, Left) -> to_left(strikead_lang_pt:to_either(X), Left).

parse_transform(Forms, Options) ->
%    error_logger:info_report({Forms}),
    Result = strikead_lang_pt:parse_transform(Forms, Options),
%    error_logger:info_report({Result}),
    Result.


