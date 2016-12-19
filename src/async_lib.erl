-module(async_lib).

-export([
         chain/2
        ]).

-export_type([
              f/0
             ]).

-type f() :: fun((any()) -> {ok | done | error, any()}).

-spec chain([f()], any()) -> any() | {error, _Reason}.
chain([], Arg) -> Arg;
chain([Fun | Funs], Arg) ->
    case Fun(Arg) of
        {ok,     V} -> chain(Funs, V);
        {done,   V} -> V;
        {error,  E} -> {error, E}
    end.
