-module(strikead_flog).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/3, log/2, format_tsv/2, format_terms/2, stop/1]).

start_link(Name, Location, Format) when is_atom(Name)->
    R = {ok, _} = gen_server:start_link({local, Name}, ?MODULE, [Location, Format], []),
    error_logger:info_report("flog " ++ atom_to_list(Name) ++" started at " ++ Location),
    R.

log(Name, A) -> gen_server:call(Name, {log, A}).

stop(Name) ->
    gen_server:cast(Name, stop).

init(State) -> {ok, State}.

current_log(Location) ->
    File = Location ++ "/" ++ strikead_calendar:format("yyyy-MM-dd/HH", calendar:universal_time()) ++ ".log",
    filelib:ensure_dir(File),
    File.


handle_call({log, List}, _From, State=[Location, Format]) ->
    File = current_log(Location),
    case strikead_autofile:using(File, [append], fun(F) -> Format(F, List) end) of
        {ok, _} -> done;
        {error, E} -> error_logger:error_msg("cannot open ~p:~p~n", [File, E])
    end,
    {reply, ok, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
code_change(_Old, State, _Extra) -> {ok, State}.
terminate(normal, State) -> error_logger:info_report({stopped, State}), ok;
terminate(Reason, _State) -> error_logger:error_report({terminated, Reason}), ok.


tsv_format_string([H|T]) -> lists:foldl( fun(X, S)  -> S  ++ "\t" ++ tsv_symbol(X) end, tsv_symbol(H), T) ++ "~n".

tsv_symbol(X) when is_binary(X); is_atom(X) -> "~s";
tsv_symbol([X|_]) when is_number(X) -> "~s";
tsv_symbol(_) -> "~5000p".

format_tsv(IoDevice, List)->
    io:format(IoDevice, tsv_format_string(List), List).

format_terms(IoDevice, Terms)->
    io:format(IoDevice, "~5000p~n", [Terms]).

