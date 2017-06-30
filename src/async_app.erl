-module(async_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(normal, list()) -> {ok, pid()} | {error, _Reason}.
start(_StartType, _StartArgs) ->
    code:load_file(async),
    async_sup:start_link().

-spec stop(atom()) -> ok.
stop(_App) -> ok.
