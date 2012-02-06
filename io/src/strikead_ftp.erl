-module(strikead_ftp).

-export([nlist/1, nlist/2, nlist_filter/2, nlist_filter/3, find/2, download/4, download/3, download/6]).

nlist(Pid) ->
    case ftp:nlist(Pid) of
        {ok, Listing} -> {ok, string:tokens(Listing, "\r\n")};
        X -> X
    end.

nlist(Pid, Path) ->
    case ftp:nlist(Pid, Path) of
        {ok, Listing} -> {ok, string:tokens(Listing, "\r\n")};
        X -> X
    end.

nlist_filter(Pid, Mask) when is_list(Mask) ->
    nlist_filter(Pid, strikead_file:compile_mask(Mask));

nlist_filter(Pid, Filter) when is_function(Filter) ->
    case nlist(Pid) of
        {ok, List} -> {ok, lists:filter(Filter, List)};
        X -> X
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
        ok = ftp:user(Pid, Login, Password),
        download(Pid, Dest, Path, Filter)
    end).

download(Pid, Dest, Path, Mask) when is_list(Mask) ->
    download(Pid, Dest, Path, strikead_file:compile_mask(Mask));

download(Pid, Dest, Path, Filter) when is_function(Filter) ->
    download(Pid, Dest, nlist_filter(Pid, Path, Filter)).

download(_Pid, _Dest, []) -> ok;

download(Pid, Dest, [{F,DF}|T]) ->
    DestFile = Dest ++ "/" ++ DF,
    ok = filelib:ensure_dir(DestFile),
    case ftp:recv(Pid, F, DestFile) of
        ok -> download(Pid, Dest, T);
        error -> error
    end;
download(Pid, Dest, [F|T]) -> download(Pid, Dest, [{F,lists:last(string:tokens(F, "/"))} | T]).
