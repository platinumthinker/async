-module(async_lib).

-export([
         error_monad/2
        ]).

-export_type([
              f/0
             ]).

-type f() :: fun((any()) -> {ok | done | error, any()}).

-spec error_monad([f()], any()) -> any() | {error, _Reason}.
error_monad([], Arg) -> Arg;
error_monad([Fun | Funs], Arg) ->
    case Fun(Arg) of
        {ok,    V} -> error_monad(Funs, V);
        {done,  V} -> V;
        {error, E} -> {error, E}
    end.
