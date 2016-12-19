-module(async_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(normal, list()) -> {ok | error, _Reason}.
start(_StartType, _StartArgs) ->
    async_sup:start_link().

-spec stop(atom()) -> ok.
stop(_App) -> ok.
