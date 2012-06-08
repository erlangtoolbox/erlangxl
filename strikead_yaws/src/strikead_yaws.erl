-module(strikead_yaws).

-include_lib("yaws/include/yaws_api.hrl").

-export([get/1, get/2, list/1, any/2, as/2, opt/1, opt/2, opt/3, params/2,
	errors/1, parse_params/1, log/3, clear_parse_caches/0, convert/2,
	find_cookie/2, find_cookie/3, host/1]).

-type parameter() :: {atom(), any()}.
-type extractor() :: fun((#arg{}) -> error_m:monad(parameter())).

-spec get/1 :: (atom()) -> extractor().
get(Name) -> get(Name, io_lib:format("Parameter '~p' must be present", [Name])).

-spec get/2 :: (atom(), string()) -> extractor().
get(Name, Message) ->
    fun(Args) ->
        case yaws_api:getvar(Args, Name) of
            undefined -> {error, Message};
            {ok, X} -> {ok, {Name, X}};
            X when is_tuple(X) ->
				{error, strikead_string:format("Single parameter '~p' expected", [Name])}
        end
    end.

-spec list/1 :: (atom()) -> extractor().
list(Name) ->
	fun(Args) ->
		erlang:display(Args#arg.querydata),
		case yaws_api:getvar(Args, Name) of
			undefined -> {ok, {Name, []}};
			{ok, X} -> {ok, {Name, [X]}};
			X when is_tuple(X) -> {ok, {Name, tuple_to_list(X)}}
		end
	end.

-spec opt/1 :: (atom()) -> extractor().
opt(Name) -> opt(Name, undefined).

-spec opt/2 :: (atom(), fun((string())-> any())) -> extractor().
opt(Name, Transform) when is_function(Transform)->
	opt(Name, undefined, Transform);
opt(Name, Default) -> opt(Name, Default, fun(X) -> X end).

-spec opt/3 :: (atom(), any(), fun((string())-> any())) -> extractor().
opt(Name, Default, Transform) ->
    fun(Args) ->
        case yaws_api:getvar(Args, Name) of
            undefined -> {ok, {Name, Default}};
            {ok, X} -> {ok, {Name, Transform(X)}};
            X when is_tuple(X) ->
				{error, strikead_string:format( "Single parameter '~p' expected", [Name])}
        end
    end.

-spec any/2 :: ([extractor()], string()) -> extractor().
any(Alts, Message) ->
    fun(Args) ->
        case find(Alts, Args) of
            undefined -> {error, Message};
            X -> X
        end
    end.

-spec find/2 :: ([extractor()], #arg{}) -> maybe_m:monad(parameter()).
find(Alts, Args) ->
    strikead_stream:mapfind(
        fun(F) ->
            case F(Args) of
                {error, _} -> undefined;
                X -> X
            end
        end, strikead_stream:to_stream(Alts)).

-spec as/2 :: (atom(), extractor()) -> extractor().
as(Name, F) ->
    fun(Args) ->
        case F(Args) of
            {ok, {_, X}} -> {ok, {Name, X}};
            X -> X
        end
    end.

-spec convert/2 :: (extractor(), fun((any()) -> any())) -> extractor().
convert(F, ConvF) ->
	fun(Args) ->
		case F(Args) of
			{ok, {Name, X}} -> {ok, {Name, ConvF(X)}};
			X -> X
		end
	end.

-spec params/2 :: ([extractor()], #arg{}) -> {[parameter()], [string()]}.
params(Fs, Args) ->
    {Params, Errors} = lists:partition(
		fun({X, _}) -> X == ok end,
		lists:foldl(fun(F, Acc) -> [F(Args) | Acc] end, [], Fs)),
    {
		lists:map(fun({_, P}) -> P end, Params),
		lists:map(fun({_, E}) -> E end, Errors)
	}.

-spec errors([string()]) -> [any()].
errors(Errors) ->
	[
		{status, 400},
		{ehtml, {html, [],
			lists:flatten(lists:map(fun(X) -> [X, {br}] end, Errors))}}
	].

-spec parse_params(#arg{}) -> [{atom(), list()}] | undefined.
parse_params(Args) ->
    Atomize = fun({K, V}) -> {list_to_atom(K), V} end,
    case (Args#arg.req)#http_request.method of
        'POST' ->
			lists:map(Atomize, yaws_api:parse_query(Args) ++
			yaws_api:parse_post(Args));
        'GET' -> lists:map(Atomize, yaws_api:parse_query(Args));
        _ -> undefined
    end.

-spec log(atom(), #arg{}, [term()]) -> ok.
log(Flog, Args, List) ->
	Ip = string:join([integer_to_list(X) || X <-
		tuple_to_list(element(1, Args#arg.client_ip_port))], "."),
	strikead_flog:log(Flog, [Ip, Args#arg.headers#headers.user_agent] ++ List).

-spec clear_parse_caches/0 :: () -> ok.
clear_parse_caches() ->
	erase(query_parse),
	erase(post_parse),
	ok.

-spec find_cookie/3 :: (string(), #arg{}, fun(() -> any()) | term()) -> any().
find_cookie(Name, Args, Default) ->
	case find_cookie(Name, Args) of
		undefined when is_function(Default) -> Default();
		undefined -> Default;
		{ok, X} -> X
	end.

-spec find_cookie/2 :: (string(), #arg{}) -> maybe_m:monad(string()).
find_cookie(Name, Args) ->
	case yaws_api:find_cookie_val(Name, Args#arg.headers#headers.cookie) of
		"" -> undefined;
		Value -> {ok, Value}
	end.

-spec host/1 :: (#arg{}) -> string().
host(Args) -> (yaws_api:redirect_self(Args))#redir_self.host.

