-module(strikead_ftp).

-export([nlist/1, nlist/2, nlist_filter/2, nlist_filter/3, find/2, download/4, download/3, download/6, ftp_error/2, user/3]).

nlist(Pid) ->
    case ftp:nlist(Pid) of
        {ok, Listing} -> {ok, string:tokens(Listing, "\r\n")};
        E -> ftp_error(E, none)
    end.

nlist(Pid, Path) ->
    case ftp:nlist(Pid, Path) of
        {ok, Listing} -> {ok, string:tokens(Listing, "\r\n")};
        E -> ftp_error(E, Path)
    end.

nlist_filter(Pid, Mask) when is_list(Mask) ->
    case nlist(Pid) of
        {ok, List} -> {ok, lists:filter(strikead_file:compile_mask(Mask), List)};
        E -> E
    end.

nlist_filter(Pid, Path, Mask) when is_list(Mask) ->
    nlist_filter(Pid, Path, strikead_file:compile_mask(Mask));

nlist_filter(Pid, Path, Filter) when is_function(Filter) ->
    case nlist(Pid, Path) of
        {ok, List} -> {ok, lists:filter(Filter, List)};
        X -> X
    end.

find(Pid, Mask) when is_list(Mask) ->
    case nlist_filter(Pid, Mask) of
        {ok, [F|_]} -> {ok, F};
        _ -> not_found
    end.

download(Host, Login, Password, Dest, Path, Mask) when is_list(Mask) ->
    download(Host, Login, Password, Dest, Path, strikead_file:compile_mask(Mask));

download(Host, Login, Password, Dest, Path, Filter) when is_function(Filter) ->
    strikead_auto:using(ftp, Host, fun(Pid)->
        ok = user(Pid, Login, Password),
        download(Pid, Dest, Path, Filter)
    end).

download(Pid, Dest, Path, Mask) when is_list(Mask) ->
    download(Pid, Dest, Path, strikead_file:compile_mask(Mask));

download(Pid, Dest, Path, Filter) when is_function(Filter) ->
    download(Pid, Dest, nlist_filter(Pid, Path, Filter)).

download(_Pid, _Dest, []) -> ok;

download(Pid, Dest, [{F,DF}|T]) ->
    ok = strikead_file:mkdirs(Dest),
	DestFile = Dest ++ "/" ++ DF,
    case ftp:recv(Pid, F, DestFile) of
        ok -> download(Pid, Dest, T);
        E -> ftp_error(E)
    end;
download(Pid, Dest, [F|T]) -> download(Pid, Dest, [{F,lists:last(string:tokens(F, "/"))} | T]).

ftp_error(E = {error, Code}) ->
	case strikead_io:is_posix_error(E) of
		true -> strikead_io:posix_error(E);
		_ -> {error, Code, ftp:formaterror(E)}
	end.

ftp_error(E = {error, Code}, Target) ->
	case strikead_io:is_posix_error(E) of
		true -> strikead_io:posix_error(E, Target);
		_ -> {error, Code, ftp:formaterror(E), Target}
	end.

user(Pid, Login, Password) ->
	case ftp:user(Pid, Login, Password) of
		ok -> ok;
		E -> ftp_error(E)
	end.
