%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(xl_file).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, do}).

-behaviour(xl_autoresource).
-export([auto_open/1, auto_close/1, using/3, rename/2, wildcards/1, write_term/2, delete_filtered/2, read_link_info/1, write_file_info/2, md5/1]).
-export([list_dir/2, compile_mask/1, find/2, exists/1, mkdirs/1, write_terms/2,
    read_terms/1, read_files/1, read_files/2, copy_if_exists/2, copy_filtered/3,
    absolute/1]).
-export([read_file/1, delete/1, make_symlink/2, write_file/2, ensure_dir/1,
    list_dir/1, copy/2, open/2, close/1, change_mode/2, read_file_info/1]).


list_dir(Dir, Filter) when is_function(Filter) ->
    case list_dir(Dir) of
        {ok, Listing} -> {ok, lists:filter(Filter, Listing)};
        E -> E
    end;

list_dir(Dir, Mask) when is_list(Mask) -> list_dir(Dir, compile_mask(Mask)).

list_dir(Dir) -> xl_io:apply_io(file, list_dir, [Dir]).

find(Dir, Mask) when is_list(Mask) ->
    case list_dir(Dir, Mask) of
        {ok, [F | _]} -> {ok, F};
        {ok, []} -> not_found;
        E -> E
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

-spec exists/1 :: (file:name()) -> error_m:monad(boolean()).
exists(Path) ->
    case xl_io:apply_io(file, read_file_info, [Path]) of
        {ok, _} -> {ok, true};
        {error, {enoent, _, _}} -> {ok, false};
        E -> E
    end.

ensure_dir(Path) -> xl_io:apply_io(filelib, ensure_dir, [Path]).

mkdirs(Path) -> ensure_dir(filename:join(Path, "X")).

-spec read_terms/1 :: (file:name()) -> {ok, [term()]} | xl_io:posix_error().
read_terms(Filename) -> xl_io:apply_io(file, consult, [Filename]).

write_term(File, T) -> write_terms(File, [T]).

write_terms(File, L) ->
    R = do([error_m ||
        ensure_dir(File),
        using(File, [write], fun(F) ->
            lists:foreach(fun(T) ->
                io:format(F, "~300p.~n", [T])
            end, L)
        end)
    ]),
    case R of
        {ok, ok} -> ok;
        X -> X
    end.


-spec copy(file:filename(), file:filename()) -> error_m:monad(ok).
copy(Src, Dst) ->
    case read_link_info(Src) of
        {ok, Info = #file_info{type = regular}} ->
            DestinationFile = filename:join(Dst, filename:basename(Src)),
            do([error_m ||
                ensure_dir(DestinationFile),
                xl_io:apply_io(file, copy, [Src, DestinationFile]),
                write_file_info(DestinationFile, Info)
            ]);
        {ok, Info = #file_info{type = directory}} ->
            do([error_m ||
                Files <- list_dir(Src),
                NewDst <- return(filename:join(Dst, filename:basename(Src))),
                mkdirs(NewDst),
                xl_lists:eforeach(fun(F) ->
                    copy(filename:join(Src, F), NewDst)
                end, Files),
                write_file_info(NewDst, Info)
            ]);
        {ok, Info = #file_info{type = symlink}} ->
            DestinationFile = filename:join(Dst, filename:basename(Src)),
            do([error_m ||
                ensure_dir(DestinationFile),
                xl_io:apply_io(file, copy, [Src, DestinationFile]),
                write_file_info(DestinationFile, Info)
            ]);
        {ok, #file_info{type = T}} -> {error, {cannot_copy, T, [Src, Dst]}};
        E -> E
    end.

type(Path) ->
    case read_link_info(Path) of
        {ok, #file_info{type = T}} -> {ok, T};
        E -> E
    end.

-spec copy_filtered(file:name(), [string()], file:name()) -> error_m:monad(ok).
copy_filtered(SrcDir, Wildcards, DstDir) ->
    xl_lists:eforeach(fun(F) ->
        xl_file:copy(F, DstDir)
    end, [F || WC <- Wildcards, F <- filelib:wildcard(SrcDir ++ "/" ++ WC)]).

-spec delete_filtered(file:name(), [string()]) -> error_m:monad(ok).
delete_filtered(Dir, Wildcards) ->
    xl_lists:eforeach(fun(F) ->
        xl_file:delete(F)
    end, [F || WC <- Wildcards, F <- filelib:wildcard(Dir ++ "/" ++ WC)]).

-spec copy_if_exists/2 :: (file:name(), file:name()) -> error_m:monad(ok).
copy_if_exists(Src, Dst) ->
    case xl_file:exists(Src) of
        {ok, true} -> copy(Src, Dst);
        {ok, _} -> ok;
        E -> E
    end.

read_file(Path) -> xl_io:apply_io(file, read_file, [Path]).
write_file(Path, Data) ->
    case ensure_dir(Path) of
        ok -> xl_io:apply_io(file, write_file, [Path, Data]);
        E -> E
    end.
open(File, Mode) -> xl_io:apply_io(file, open, [File, Mode]).
close(Fd) -> xl_io:apply_io(file, close, [Fd]).
make_symlink(Target, Link) -> xl_io:apply_io(file, make_symlink, [Target, Link]).
write_file_info(Path, Info) -> xl_io:apply_io(file, write_file_info, [Path, Info]).
read_file_info(Path) -> xl_io:apply_io(file, read_file_info, [Path]).
read_link_info(Path) -> xl_io:apply_io(file, read_link_info, [Path]).
change_mode(Path, Mode) -> xl_io:apply_io(file, change_mode, [Path, Mode]).
rename(From, To) -> xl_io:apply_io(file, rename, [From, To]).

absolute(Path) ->
    Abs = lists:reverse(lists:filter(fun(X) -> X /= "." end,
        filename:split(filename:join([filename:absname(Path)])))),
    filename:join(absolute(Abs, [], 0)).

absolute([], Acc, _) -> Acc;
absolute([".." | T], Acc, Skip) -> absolute(T, Acc, Skip + 1);
absolute([H | T], Acc, 0) -> absolute(T, [H | Acc], 0);
absolute(["/"], Acc, _) -> ["/" | Acc];
absolute([_ | T], Acc, Skip) -> absolute(T, Acc, Skip - 1).

-spec read_files/1 :: ([string()]) -> error_m:monad([{string(), binary()}]).
read_files(Wildcards) -> read_files(Wildcards, name).

-spec read_files/2 :: ([string()], name | {base, file:name()}) ->
    error_m:monad([{string(), binary()}]).
read_files(Wildcards, Option) ->
    xl_lists:eflatten(xl_lists:emap(fun(Name) ->
        case type(Name) of
            {ok, directory} -> read_files([Name ++ "/*"], Option);
            {ok, regular} ->
                case read_file(Name) of
                    {ok, Bin} ->
                        N = case Option of
                            name ->
                                lists:last(filename:split(Name));
                            {base, BaseDir} ->
                                AbsBase = absolute(BaseDir),
                                AbsName = absolute(Name),
                                string:substr(AbsName, string:len(AbsBase) + 2);
                            _ -> {error, {badarg, Option}}
                        end,
                        {ok, {N, Bin}};
                    E -> E
                end;
            {ok, T} -> {error, {cannot_read, T, Name}};
            E -> E
        end
    end, wildcards(Wildcards))).

delete(Path) ->
    case type(Path) of
        {ok, regular} ->
            xl_io:apply_io(file, delete, [Path]);
        {ok, symlink} ->
            xl_io:apply_io(file, delete, [Path]);
        {ok, directory} ->
            do([error_m ||
                Files <- list_dir(Path),
                xl_lists:eforeach(fun(P) -> delete(filename:join(Path, P)) end, Files),
                xl_io:apply_io(file, del_dir, [Path])
            ]);
        {ok, T} -> {error, {cannot_delete, T, [Path]}};
        {error, {enoent, _, _}} -> ok;
        E -> E
    end.

wildcards(Wildcards) -> [Filename || Wildcard <- Wildcards, Filename <- filelib:wildcard(Wildcard)].

-spec(md5(file:name()) -> error_m:monad(string())).
md5(Path) ->
    case read_file(Path) of
        {ok, Bytes} -> {ok, xl_codec:md5(Bytes)};
        E -> E
    end.
%%
% autoresource
%%
auto_open([File, Mode]) -> open(File, Mode).
auto_close(D) -> close(D).
using(File, Mode, F) -> xl_auto:using(?MODULE, [File, Mode], F).
