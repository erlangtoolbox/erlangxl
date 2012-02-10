-module(strikead_file).

-export([list_dir/2, compile_mask/1, find/2, exists/1, ensure_dir/1, mkdirs/1]).

list_dir(Dir, Filter) when is_function(Filter) ->
    case file:list_dir(Dir) of
        {ok, Listing} -> {ok, lists:filter(Filter, Listing)};
        E -> strikead_io:posix_error(E, Dir)
    end;

list_dir(Dir, Mask) when is_list(Mask) -> list_dir(Dir, compile_mask(Mask)).

find(Dir, Mask) when is_list(Mask) ->
    case list_dir(Dir, Mask) of
        {ok, [F|_]} -> {ok, F};
        {ok, []} -> not_found;
        X -> X
    end.

compile_mask(Mask) ->
    fun(X) ->
        case re:run(X, compile_mask(Mask, [])) of
            {match, _} -> true;
                _ -> false
        end
    end.
compile_mask([], Acc) -> Acc;
compile_mask([$. | T], Acc) -> compile_mask(T, Acc ++ "\.");     
compile_mask([$* | T], Acc) -> compile_mask(T, Acc ++ ".*");     
compile_mask([$? | T], Acc) -> compile_mask(T, Acc ++ ".");     
compile_mask([H | T], Acc) -> compile_mask(T, Acc ++ [H]).     

exists(Path) ->
	case file:read_file_info(Path) of
		{ok, _} -> true;
		{error, enoent} -> false;
		E -> strikead_io:posix_error(E, Path)
	end.

ensure_dir(Path) ->
	case filelib:ensure_dir(Path) of
		ok -> ok;
		E -> strikead_io:posix_error(E, Path)
	end.

mkdirs(Path) -> ensure_dir(Path ++ "/X").
