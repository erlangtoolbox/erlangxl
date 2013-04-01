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
-module(xl_yaws_errors).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([out404/3, out404/1, crashmsg/3]).

out404(Args) -> out404(Args, get(gc), get(sc)).
out404(Args, _GC, _SC) ->
    {abs_path, Path} = (Args#arg.req)#http_request.path,
    B = html(Path),
    [{status, 404},
        {header, {content_type, "text/html"}},
        {header, {connection, "close"}},
        {html, B}].



html(Path) ->
    L = ["<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">"
    "<HTML><HEAD>"
    "<TITLE>404 Not Found</TITLE>"
    "</HEAD><BODY>"
    "<H1>Not Found</H1>"
    "The requested URL ", yaws_api:htmlize(Path), " was not found on this server.<P>"
    "<HR>",
        yaws:address(),
        " </BODY></HTML>"
    ], list_to_binary(L).




crashmsg(_Arg, _SC, L) ->
    {html, "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">"
    "<HTML><HEAD>"
    "<TITLE>500 Internal Server Error</TITLE>"
    "</HEAD><BODY>"
    "<H1>Internal Server Error</H1>"
    "<HR><PRE>" ++ L ++ "</PRE><HR>" ++ yaws:address() ++ "</BODY></HTML>"}.


