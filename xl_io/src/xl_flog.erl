-module(xl_flog).

-compile({parse_transform, do}).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/3, log/2, format_tsv/2, format_terms/2, format_tsv_timestamp/2, stop/1, flush/1]).

-import(error_logger, [info_msg/2, error_msg/2]).

-record(state, {name, location, format, fd, current}).

start_link(Name, Location, Format) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Location, Format}, []).

log(Name, A) -> gen_server:cast(Name, {log, A}).

stop(Name) ->
    gen_server:cast(Name, stop).

flush(Name) ->
    gen_server:call(Name, flush).

init({Name, Location, Format}) ->
    info_msg("starting log ~p at ~s", [Name, Location]),
    case update_state(#state{name = Name, location = Location, format = Format}) of
        {ok, State} -> {ok, State};
        {error, X} -> {stop, X}
    end.

handle_call(flush, _From, State = #state{fd = Fd}) ->
    close(Fd),
    {reply, ok, State#state{current = undefined, fd = undefined}};
handle_call(_Req, _From, State) -> {noreply, State}.

handle_cast({log, List}, S) ->
    case update_state(S) of
        {ok, State = #state{fd = Fd, format = Format}} ->
            Format(Fd, List),
            {noreply, State};
        {error, E} ->
            error_msg("~p:~p~n", [E, S]),
            {noreply, S}
    end;

handle_cast(stop, State = #state{fd = Fd}) ->
    close(Fd),
    {stop, normal, State#state{current = undefined, fd = undefined}};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.
code_change(_Old, State, _Extra) -> {ok, State}.
terminate(_Reason, #state{fd = Fd}) -> close(Fd), ok.


tsv_format_string([H | T]) -> lists:foldl(fun(X, S) -> S ++ "\t" ++ tsv_symbol(X) end, tsv_symbol(H), T) ++ "~n".

tsv_symbol(X) when is_binary(X); is_atom(X); is_list(X) andalso length(X) == 0 -> "~s";
tsv_symbol([X | _]) when is_number(X) -> "~s";
tsv_symbol(_) -> "~5000p".

format_tsv(IoDevice, List) ->
    io:format(IoDevice, tsv_format_string(List), List).

format_tsv_timestamp(IoDevice, List) -> format_tsv(IoDevice, [xl_calendar:format("yyyy-MM-dd HH:mm:ss", calendar:universal_time()) | List]).

format_terms(IoDevice, Terms) ->
    io:format(IoDevice, "~5000p~n", [Terms]).


update_state(State = #state{location = Location, current = Current, fd = Fd}) ->
    case Location ++ "/" ++ xl_calendar:format("yyyy-MM-dd/HH", calendar:universal_time()) ++ ".log" of
        Current -> {ok, State};
        NewLocation ->
            do([error_m ||
                xl_file:ensure_dir(NewLocation),
                close(Fd),
                NewFd <- xl_file:open(NewLocation, [append, delayed_write]),
                return(State#state{current = NewLocation, fd = NewFd})
            ])
    end.

close(undefined) -> ok;
close(Fd) ->
    case xl_file:close(Fd) of
        ok -> ok;
        X -> error_msg("cannot close file: ~p", X), X
    end.

