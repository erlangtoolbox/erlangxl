-module(strikead_qlc_auto).

-behavoiur(strikead_autoresource).

-export([auto_open/1, auto_close/1, using/3]).

auto_open({cursor, Args}) -> {ok, qlc:cursor(Args)};
auto_open(X) -> {error, {badarg, X}}.

auto_close(C = {qlc_cursor, _}) -> qlc:delete_cursor(C);
auto_close(X) -> {error, {badarg, X}}.

using(What, Args, F) -> strikead_auto:using(?MODULE, {What, Args}, F).
