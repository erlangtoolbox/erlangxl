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
-module(pl_persist_test).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").

-record(testobj, {
    id,
    name
}).

persist_test() ->
    {ok, P} = open(),
    T1 = #testobj{id = "1", name = "n1"},
    T2 = #testobj{id = "2", name = "n2"},
    ?assertEqual(ok, pl_persist:mutate(P, fun(Data) ->
        NewState = pl_persist_data:store(Data, [T1, T2]),
        {NewState}
    end)),
    ?assertEqual([T1, T2], pl_persist:read(P, fun(Data) ->
        pl_persist_data:values(Data)
    end)),
    ?assertEqual([T1, T2], pl_persist:read(P, fun(Data) ->
        pl_persist_data:values(Data)
    end)).
%%     ?assertEqual([T1, T2], persist:select(P)),
%%     ?assertEqual({ok, T1}, persist:get(P, "1")),
%%     ?assertEqual({ok, T2}, persist:get(P, "2")),
%%     ?assertEqual(undefined, persist:get(P, "3")),
%%
%%     persist:delete(P, "1"),
%%     ?assertEqual([T2], persist:select(P)),
%%     ?assertEqual(undefined, persist:get(P, "1")),
%%     ?assertEqual({ok, T2}, persist:get(P, "2")),
%%
%%     ?assertEqual(ok, persist:close(P)).

open() ->
    pl_persist:open(persist:by_index(#testobj.id), [
        {data_backend, pl_persist_data_orddict}
    ]).
