-module(strikead_file).

-export([list_dir/2, compile_mask/1, find/2, exists/1, write_terms/2]).

list_dir(Dir, Filter) when is_function(Filter) ->
    case file:list_dir(Dir) of
        {ok, Listing} -> {ok, lists:filter(Filter, Listing)};
        X -> X
    end;

list_dir(Dir, Mask) when is_list(Mask) -> list_dir(Dir, compile_mask(Mask)).

find(Dir, Mask) when is_list(Mask) ->
    case list_dir(Dir, Mask) of
        {ok, [F|_]} -> {ok, F};
        _ -> not_found
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
    {R, _Info} = file:read_file_info(Path),
    R == ok.

write_terms(File, L) when is_list(L) ->
    strikead_autofile:using(File, [write], fun(F) ->
        lists:foreach(fun(X) -> io:format(F, "~p.~n",[X]) end, L)
    end);

write_terms(File, L) -> write_terms(File, [L]).

