-module(async_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args, Type),
        {I, {I, start_link, Args}, permanent, 2000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init([]) -> {ok, _}.
init([]) ->
    {ok, { {one_for_all, 10, 1}, [
                                 ?CHILD(async_server, [], worker),
                                 ?CHILD(async_pool, [2], supervisor)
                                 % ?CHILD(async_compiler, worker),
                                 % ?CHILD(async_notifier, worker)
                                ]} }.

