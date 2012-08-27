-module(strikead_yaws_errors).

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


