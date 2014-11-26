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
-module(xl_tdb_storage).

-compile({parse_transform, do}).

-export([load/3, store/3, delete/2]).

-spec(load(file:name(), pos_integer(), [{pos_integer(), fun((term()) -> term())}]) -> error_m:monad([term()])).
load(Location, Version, Migrations) ->
    do([error_m ||
        xl_file:mkdirs(Location),
        fix_format(Location),
        perform_migration(Location, Version, Migrations),
        Files <- xl_file:list_dir(Location, "*.tdb"),
        xl_lists:emap(fun(F) ->
            case xl_file:read_terms(filename:join(Location, F)) of
                {ok, [Content]} -> {ok, Content};
                E -> E
            end
        end, lists:sort(Files))
    ]).

-spec(store(file:name(), xl_string:iostring(), term()) -> error_m:monad(ok)).
store(Location, Id, X) ->
    xl_file:write_term(xl_string:join([Location, "/", Id, ".tdb"]), X).

-spec(delete(file:name(), xl_string:iostring()) -> error_m:monad(ok)).
delete(Location, Id) ->
    xl_file:delete(xl_string:join([Location, "/", Id, ".tdb"])).

version(VersionFile) ->
    case xl_file:exists(VersionFile) of
        {ok, true} ->
            case xl_file:read_terms(VersionFile) of
                {ok, [{version, Version}]} -> {ok, Version};
                E -> E
            end;
        {ok, false} -> {ok, undefined};
        E -> E
    end.

fix_format(Location) ->
    VersionFile = filename:join(Location, ".version"),
    BackupTag = xl_calendar:now_micros(),
    case version(VersionFile) of
        {ok, undefined} -> ok;
        {ok, Version} ->
            case backup(Location, BackupTag) of
                ok ->
                    try do([error_m ||
                        Files <- xl_file:list_dir(Location, "*.tdb"),
                        xl_lists:eforeach(fun(F) ->
                            Filename = filename:join(Location, F),
                            do([error_m ||
                                Content <- xl_file:read(Filename),
                                xl_file:write_term(Filename, xl_lang:insert_element(3, binary_to_term(Content), Version))
                            ])
                        end, Files),
                        xl_file:delete(VersionFile)
                    ]) of
                        ok -> ok;
                        E ->
                            case restore(Location, BackupTag) of
                                ok -> E;
                                RE -> RE
                            end
                    catch
                        _:E ->
                            case restore(Location, BackupTag) of
                                ok -> {error, E};
                                RE -> RE
                            end
                    end;
                E -> E
            end;
        E -> E
    end.

perform_migration(Location, TargetVersion, Migrations) ->
    BackupTag = xl_calendar:now_micros(),
    backup(Location, BackupTag),
    try do([error_m ||
        Files <- xl_file:list_dir(Location, "*.tdb"),
        xl_lists:eforeach(fun(F) ->
            Filename = filename:join(Location, F),
            do([error_m ||
                [{Id, Object, Version, LastModified, Deleted}] <- xl_file:read_terms(Filename),
                Migrated <- xl_lists:efoldl(fun({_, M}, T) ->
                    M(T)
                end, Object, lists:dropwhile(fun({V, _M}) -> V =< Version end, Migrations)),
                xl_file:write_term(Filename, {Id, Migrated, TargetVersion, LastModified, Deleted})
            ])
        end, Files)
    ]) of
        ok -> ok;
        E ->
            case restore(Location, BackupTag) of
                ok -> E;
                RE -> RE
            end
    catch
        _:E ->
            case restore(Location, BackupTag) of
                ok -> {error, E};
                RE -> RE
            end
    end.

backup(Location, BackupTag) ->
    BackupLocation = filename:join([Location, "backup", integer_to_list(BackupTag)]),
    case xl_file:list_dir(Location, "*.tdb") of
        {ok, [_ | _]} ->
            do([error_m ||
                xl_file:mkdirs(BackupLocation),
                xl_file:copy_filtered(Location, ["*.tdb"], BackupLocation)
            ]);
        {ok, _} -> ok;
        E -> E
    end.

restore(Location, BackupTag) ->
    BackupLocation = filename:join([Location, "backup", integer_to_list(BackupTag)]),
    case xl_file:exists(BackupLocation) of
        {ok, true} ->
            do([error_m ||
                xl_file:delete_filtered(Location, ["*.tdb"]),
                xl_file:copy_filtered(BackupLocation, ["*.tdb"], Location)
            ]);
        {ok, false} -> ok;
        E -> E
    end.
