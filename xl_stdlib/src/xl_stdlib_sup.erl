%% Copyright
-module(xl_stdlib_sup).
-author("volodymyr.kyrychenko@strikead.com").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
