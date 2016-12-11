-module(async_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args, Type),
        {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init([]) -> supervisor:child_spec().
init([]) ->
    {ok, { {one_for_one, 1, 5}, [
                                 ?CHILD(async_server, [], worker),
                                 ?CHILD(async_pool, [2], worker)
                                 % ?CHILD(async_compiler, worker),
                                 % ?CHILD(async_notifier, worker)
                                ]} }.

