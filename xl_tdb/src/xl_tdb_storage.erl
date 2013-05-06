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

-export([load/1, store/3, delete/2]).

-spec(load(file:name()) -> error_m:monad([term()])).
load(Location) ->
    do([error_m ||
        xl_file:mkdirs(Location),
        Files <- xl_file:list_dir(Location, "*.bin"),
        xl_lists:emap(fun(F) ->
            do([error_m ||
                Content <- xl_file:read_file(filename:join(Location, F)),
                return(binary_to_term(Content))
            ])
        end, Files)
    ]).

-spec(store(file:name(), xl_string:iostring(), term()) -> error_m:monad(ok)).
store(Location, Id, X) ->
    xl_file:write_file(xl_string:join([Location, "/", Id, ".bin"]), term_to_binary(X)).

-spec(delete(file:name(), xl_string:iostring()) -> error_m:monad(ok)).
delete(Location, Id) ->
    xl_file:delete(xl_string:join([Location, "/", Id, ".bin"])).


