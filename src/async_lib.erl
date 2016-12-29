-module(async_lib).

-export([
         chain/2,
         env/1,
         env/2
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

-spec env(atom()) -> any().
env(Prop) -> env(Prop, undefined).
-spec env(atom(), any()) -> any().
env(Prop, Default) ->
    application:get_env(async, Prop, Default).
