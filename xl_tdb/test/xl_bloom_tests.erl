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
-module(xl_bloom_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("xl_stdlib/include/xl_eunit.hrl").

bloom_test_() ->
    {timeout, 2000, fun() ->
        {ok, [{values, BinValues}]} = xl_json:to_abstract(xl_json:parse_file(xl_eunit:resource(?MODULE, "4.json"))),
        Values = [{xxx, binary_to_atom(V, utf8)} || V <- BinValues],
        Counts = xl_lists:seq(1, 5, 0.2, fun(X) -> round(math:exp(X)) end),

        lists:foreach(fun(Count) ->
            {Actual, _} = xl_lists:split(Count, Values),
            Bloom = xl_bloom:new(Actual),
            Value = lists:nth(random:uniform(length(Actual)), Actual),
            xl_lists:times(fun() ->
                xl_eunit:performance(xl_convert:make_atom(['bloom#', Count]), fun() ->
                    true = xl_bloom:contains(Value, Bloom),
                    false = xl_bloom:contains({wtf, Value}, Bloom)
                end, 1000),
                xl_eunit:performance(xl_convert:make_atom(['list#', Count]), fun() ->
                    true = lists:member(Value, Actual),
                    false = lists:member({wtf, Value}, Actual)
                end, 1000)
            end, 10)
        end, Counts)
    end}.
