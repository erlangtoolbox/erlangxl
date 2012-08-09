-module(strikead_lang).

-export([ifelse/3]).

ifelse(true, Then, _) -> Then;
ifelse(false, _, Else) -> Else.
