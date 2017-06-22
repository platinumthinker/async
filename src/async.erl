-module(async).

-export([
         watch/1,
         unwatch/1,
         pause/0,
         unpause/0,
         forget_changes/0
        ]).

-spec watch(Path :: string()) -> ok | {error, _Reason}.
watch(Path) -> async_server:watch(Path).

-spec unwatch(Path :: string()) -> ok | {error, _Reason}.
unwatch(Path) -> async_server:unwatch(Path).

-spec pause() -> ok.
pause() -> async_server:pause().
-spec unpause() -> ok.
unpause() -> async_server:unpause().
-spec forget_changes() -> ok.
forget_changes() -> async_server:forget_changes().
