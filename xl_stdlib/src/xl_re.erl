-module(xl_re).
-author("andrei.zavada@strikead.com").

-export([start/0, compile/2,
         run/2, split/2,
         run/3, split/3]).

-define(re_table, 're_store').


start() ->
    spawn(fun loop/0) ! new,
    ok.

loop() ->
    receive
        new ->
            catch ets:delete(?re_table),
            ets:new(?re_table, [named_table, public, set, {read_concurrency, true}]),
            loop();
        stop ->
            ets:delete(?re_table)
    end.


run(Subject, RE) ->
    run(Subject, RE, []).
run(Subject, RE, Options) ->
    apply_fun(Subject, RE, Options, fun re:run/3).

split(Subject, RE) ->
    split(Subject, RE, []).
split(Subject, RE, Options) ->
    apply_fun(Subject, RE, Options, fun re:split/3).

apply_fun(Subject, RE, Options, Fun) ->
    Key = erlang:phash2({RE, Options}),
    case ets:lookup(?re_table, Key) of
        [{_, MP}] ->
            Fun(Subject, MP, mp_safe(Options));
        [] ->
            case re:compile(RE, compile_safe(Options)) of
                {ok, MP} ->
                    _ = ets:insert(?re_table, {Key, MP}),
                    Fun(Subject, MP, Options);
                ErrorReason ->
                    ErrorReason
            end
    end.

compile(RE, Options) ->
    Key = erlang:phash2({RE, Options}),
    case ets:lookup(?re_table, Key) of
        [{_, MP}] ->
            {ok, MP};
        [] ->
            case re:compile(RE, compile_safe(Options)) of
                {ok, MP} ->
                    _ = ets:insert(?re_table, {Key, MP}),
                    {ok, MP};
                ErrorReason ->
                    ErrorReason
            end
    end.

compile_safe(Options) ->
    lists:filter(
      fun(unicode)              -> true;
         (anchored)	        -> true;
         (caseless)	        -> true;
         (dollar_endonly)	-> true;
         (dotall)	        -> true;
         (extended)	        -> true;
         (firstline)	        -> true;
         (multiline)	        -> true;
         (no_auto_capture)	-> true;
         (dupnames)	        -> true;
         (ungreedy)	        -> true;
         ({newline, _})         -> true;
         (_)                    -> false
      end, Options).

mp_safe(Options) ->
    lists:filter(
      fun(anchored)             -> true;
         (global)               -> true;
         (notbol)               -> true;
         (noteol)               -> true;
         (notempty)             -> true;
         ({offset, _})          -> true;
         ({newline, _})         -> true;
         ({capture, _})         -> true;
         ({capture, _, _})      -> true;
         (_)                    -> false
      end, Options).


% Local Variables:
% Mode: erlang
% indent-tabs-mode: nil
% End:
