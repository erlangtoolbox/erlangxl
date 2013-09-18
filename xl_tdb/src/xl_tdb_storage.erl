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
        perform_migration(Location, Version, Migrations),
        Files <- xl_file:list_dir(Location, "*.tdb"),
        xl_lists:emap(fun(F) ->
            case xl_file:read_file(filename:join(Location, F)) of
                {ok, Content} -> {ok, binary_to_term(Content)};
                E -> E
            end
        end, lists:sort(Files))
    ]).

-spec(store(file:name(), xl_string:iostring(), term()) -> error_m:monad(ok)).
store(Location, Id, X) ->
    xl_file:write_file(xl_string:join([Location, "/", Id, ".tdb"]), term_to_binary(X)).

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
        {ok, false} -> {ok, 0};
        E -> E
    end.

perform_migration(Location, TargetVersion, Migrations) ->
    VersionFile = filename:join(Location, ".version"),
    case version(VersionFile) of
        {ok, TargetVersion} -> ok;
        {ok, OldVersion} ->
            backup(Location, OldVersion),
            ApplicableMigrations = lists:dropwhile(fun({V, _M}) -> V =< OldVersion end, Migrations),
            try
                do([error_m ||
                    Files <- xl_file:list_dir(Location, "*.tdb"),
                    xl_lists:eforeach(fun(F) ->
                        Filename = filename:join(Location, F),
                        do([error_m ||
                            Content <- xl_file:read_file(Filename),
                            Object <- xl_lists:efoldl(fun({_, M}, T) ->
                                M(T)
                            end, binary_to_term(Content), ApplicableMigrations),
                            xl_file:write_file(Filename, term_to_binary(Object))
                        ])
                    end, Files)
                ]) of
                ok -> xl_file:write_term(VersionFile, {version, TargetVersion})                ;
                E ->
                    case restore(Location, OldVersion) of
                        ok -> E;
                        RE -> RE
                    end
            catch
                _:E ->
                    case restore(Location, OldVersion) of
                        ok -> {error, E};
                        RE -> RE
                    end
            end;
        E -> E
    end.

backup(Location, Version) ->
    BackupLocation = filename:join([Location, "backup", integer_to_list(Version)]),
    do([error_m ||
        xl_file:mkdirs(BackupLocation),
        xl_file:copy_filtered(Location, ["*.tdb"], BackupLocation)
    ]).

restore(Location, Version) ->
    BackupLocation = filename:join([Location, "backup", integer_to_list(Version)]),
    do([error_m ||
        xl_file:delete_filtered(Location, ["*.tdb"]),
        xl_file:copy_filtered(BackupLocation, ["*.tdb"], Location)
    ]).
