-module(xl_re).
-author("andrei.zavada@strikead.com").

-export([start/0, compile/2,
        run/2, split/2,
        run/3, split/3]).



-spec start() -> ok.
start() ->
    xl_state:new(?MODULE, [{read_concurrency, true}]),
    ok.


-type option() :: [term()].  %% refer to man 3 re
-type mp() :: term().
-type hash() :: integer().

-spec run(Subject :: iodata() | io:charlist(),
        RE :: mp() | iodata() | integer(),
        Options :: [option()]) ->
            {match, Captured :: [term()]} | nomatch |
            {error, not_compiled}.
run(Subject, RE) -> run(Subject, RE, []).
run(Subject, RE, Options) -> apply_fun(Subject, RE, Options, fun re:run/3).

-spec split(Subject :: iodata() | io:charlist(),
        RE :: mp() | iodata() | integer(),
        Options :: [option()]) ->
            {match, Captured :: [term()]} | nomatch |
            {error, not_compiled}.
split(Subject, RE) ->
    split(Subject, RE, []).
split(Subject, RE, Options) ->
    apply_fun(Subject, RE, Options, fun re:split/3).

-spec compile(RE :: iodata(),
        Options :: [option()]) ->
            {ok, Hash :: hash()} |
            {error, {ErrString :: string(), Position :: non_neg_integer()}}.
compile(RE, Options) ->
    Key = erlang:phash2({RE, Options}),
    case xl_state:get(?MODULE, Key) of
        {ok, _} ->
            {ok, Key};
        undefined ->
            case re:compile(RE, compile_safe(Options)) of
                {ok, MP} ->
                    _ = xl_state:set(?MODULE, Key, MP),
                    {ok, Key};
                ErrorReason ->
                    ErrorReason
            end
    end.


%% supporting functions

apply_fun(Subject, Key, Options, Fun) when is_integer(Key) ->
    case xl_state:get(?MODULE, Key) of
        {ok, MP} ->
            Fun(Subject, MP, mp_safe(Options));
        undefined ->
            {error, not_compiled}
    end;
apply_fun(Subject, RE, Options, Fun) ->
    Key = erlang:phash2({RE, Options}),
    case xl_state:get(?MODULE, Key) of
        {ok, [MP]} ->
            Fun(Subject, MP, mp_safe(Options));
        [] ->
            case re:compile(RE, compile_safe(Options)) of
                {ok, MP} ->
                    xl_state:set(?MODULE, Key, MP),
                    Fun(Subject, MP, mp_safe(Options));
                ErrorReason ->
                    ErrorReason
            end
    end.

compile_safe(Options) ->
    lists:filter(
            fun(unicode) -> true;
                (anchored) -> true;
                (caseless) -> true;
                (dollar_endonly) -> true;
                (dotall) -> true;
                (extended) -> true;
                (firstline) -> true;
                (multiline) -> true;
                (no_auto_capture) -> true;
                (dupnames) -> true;
                (ungreedy) -> true;
                ({newline, _}) -> true;
                (_) -> false
            end, Options).

mp_safe(Options) ->
    lists:filter(
            fun(anchored) -> true;
                (global) -> true;
                (notbol) -> true;
                (noteol) -> true;
                (notempty) -> true;
                ({offset, _}) -> true;
                ({newline, _}) -> true;
                ({capture, _}) -> true;
                ({capture, _, _}) -> true;
                (_) -> false
            end, Options).
