-module (async_erl).

-export([
         filetypes/0,
         init/1,
         terminate/1,
         change/2,
         compile/2,
         pre_load/2,
         after_load/2
        ]).

-record(s, {
          default_compile_opts = []
         }).

-spec filetypes() -> [async_plugin:filetype()].
filetypes() -> [ "erl" ].

-spec init([async_plugin:opt()]) -> #s{}.
init(_Opts) ->
    #s{default_compile_opts = [bin_opt_info, return, binary, verbose]}.

-spec terminate(#s{}) -> ok.
terminate(_State) -> ok.

-spec change({async_plugin:filetype(),
             async_plugin:filename(),
             async_plugin:event()}, #s{}) ->
    {ok, [{async_plugin:filetype(),
           async_plugin:filename(),
           [async_plugin:opt()]}]}.
change({"erl", _File, _Event}, #s{default_compile_opts = DefOpts}) ->
    Opts = [],
    NOpts = lists:usort(DefOpts ++ Opts),
    {ok, NOpts}.

-spec compile({async_plugin:filetype(),
              async_plugin:filename(),
              [async_plugin:opt()]}, #s{}) ->
    {ok, {module(), async_plugin:filename(), ModBin :: binary(),
          Warn :: [string()]}} |
    {error, {Err :: [string()], Warn :: [string()]}}.
compile({"erl", File, Opts}, _State) ->
    compile:file(File, Opts).

-spec pre_load(_, #s{}) -> ok.
pre_load({Module, _, _}, _) ->
    io:format("erl => module ~p recompile~n", [Module]),
    ok.

-spec after_load(_, #s{}) -> ok.
after_load(_, _) -> ok.
