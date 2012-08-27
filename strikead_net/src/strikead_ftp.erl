-module(strikead_ftp).

-compile({parse_transform, do}).

-behaviour(strikead_autoresource).
-export([auto_open/1, auto_close/1, using/4]).
-export([nlist_filter/2, nlist_filter/3, find/2, download/4, download/3, download/6, ftp_error/2]).
-export([nlist/1, nlist/2, user/3, recv/3, cd/2, recv_bin/2]).

nlist(Pid) ->
    case apply_ftp(nlist, [Pid]) of
        {ok, Listing} -> {ok, string:tokens(Listing, "\r\n")};
        E -> E
    end.

nlist(Pid, Path) ->
    case apply_ftp(nlist, [Pid, Path]) of
        {ok, Listing} -> {ok, string:tokens(Listing, "\r\n")};
        E -> E
    end.

nlist_filter(Pid, Mask) when is_list(Mask) ->
    nlist_filter(Pid, strikead_file:compile_mask(Mask));

nlist_filter(Pid, Filter) when is_function(Filter) ->
    case nlist(Pid) of
        {ok, List} -> {ok, lists:filter(Filter, List)};
        E -> E
    end.

nlist_filter(Pid, Path, Mask) when is_list(Mask) ->
    nlist_filter(Pid, Path, strikead_file:compile_mask(Mask));

nlist_filter(Pid, Path, Filter) when is_function(Filter) ->
    case nlist(Pid, Path) of
        {ok, List} -> {ok, lists:filter(Filter, List)};
        E -> E
    end.

find(Pid, Mask) when is_list(Mask) ->
    case nlist_filter(Pid, Mask) of
        {ok, [F | _]} -> {ok, F};
        {ok, []} -> not_found;
        E -> E
    end.

download(Host, Username, Password, Dest, Path, Mask) when is_list(Mask) ->
    download(Host, Username, Password, Dest, Path, strikead_file:compile_mask(Mask));

download(Host, Username, Password, Dest, Path, Filter) when is_function(Filter) ->
    using(Host, Username, Password, fun(Pid) ->
        download(Pid, Dest, Path, Filter)
    end).

download(Pid, Dest, Path, Mask) when is_list(Mask) ->
    download(Pid, Dest, Path, strikead_file:compile_mask(Mask));

download(Pid, Dest, Path, Filter) when is_function(Filter) ->
    download(Pid, Dest, nlist_filter(Pid, Path, Filter)).

download(_Pid, _Dest, []) -> ok;
download(Pid, Dest, [{F, DF} | T]) ->
    DestFile = Dest ++ "/" ++ DF,
    do([error_m ||
        strikead_file:mkdirs(Dest),
        recv(Pid, F, DestFile),
        download(Pid, Dest, T)
    ]);
download(Pid, Dest, [F | T]) -> download(Pid, Dest, [{F, lists:last(string:tokens(F, "/"))} | T]).

ftp_error(E = {error, Code}, Target) ->
    case strikead_io:is_posix_error(E) of
        true -> strikead_io:posix_error(E, Target);
        _ -> {error, {Code, ftp:formaterror(E), Target}}
    end.

user(Pid, Login, Password) -> apply_ftp(user, [Pid, Login, Password]).

recv(Pid, Source, Dest) ->
    do([error_m ||
        ok <- apply_ftp(type, [Pid, binary]),
        apply_ftp(recv, [Pid, Source, Dest])
    ]).

cd(Pid, Path) -> apply_ftp(cd, [Pid, Path]).

recv_bin(Pid, Path) -> apply_ftp(recv_bin, [Pid, Path]).

apply_ftp(Command, Args) ->
    case apply(ftp, Command, Args) of
        E = {error, _} -> ftp_error(E, Args);
        X -> X
    end.

%%
% autoresource
%%
auto_open([Host, Username, Password]) ->
    do([error_m ||
        Pid <- apply_ftp(open, [Host]),
        user(Pid, Username, Password),
        {ok, Pid}
    ]).

auto_close(Pid) -> ftp:close(Pid).

using(Host, Username, Password, F) -> strikead_auto:using(?MODULE, [Host, Username, Password], F).

