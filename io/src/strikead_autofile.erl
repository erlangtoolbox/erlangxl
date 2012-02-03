-module(strikead_autofile).

-behaviour(strikead_autoresource).

-export([open/1, close/1, using/3]).

open([File, Mode]) -> file:open(File, Mode).

close(D) -> file:close(D).

using(File, Mode, F) -> strikead_auto:using(strikead_autofile, [File, Mode], F).
