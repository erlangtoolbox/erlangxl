-module(strikead_qlc_auto).

-behavoiur(strikead_autoresource).

-export([open/1, close/1, using/3]).

open({cursor, Args}) -> {ok, qlc:cursor(Args)};
open(X) -> {badarg, X}.

close(C = {qlc_cursor, _}) -> qlc:delete_cursor(C);
close(X) -> {badarg, X}.

using(What, Args, F) -> strikead_auto:using(?MODULE, {What, Args}, F).
