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
    #s{default_compile_opts = [debug, bin_opt_info, return, binary, verbose]}.

-spec terminate(#s{}) -> ok.
terminate(_State) -> ok.

-spec change({async_plugin:filetype(),
             async_plugin:filename(),
             async_plugin:event()}, #s{}) ->
    {ok, [{async_plugin:filetype(),
           async_plugin:filename(),
           [async_plugin:opt()]}]}.
change({"erl", File, _Event}, #s{default_compile_opts = DefOpts}) ->
    Module = list_to_atom(filename:basename(File, ".erl")),
    Opts = case code:is_loaded(Module) of
        {file, _} ->
            ComOpts = Module:module_info(compile),
            Opts1 = proplists:get_value(options, ComOpts, []),
            lists:filtermap(
              fun({out_dir, _}) ->
                      OutDir = filename:dirname(code:which(Module)),
                      {true, {out_dir, OutDir}};
                 ({i, _}) -> false;
                 (encrypt_debug_info) -> false;
                 (debug_info) -> false;
                 (_) -> true end, Opts1);
        _ -> []
    end,

    OptsInc = include_dirs_opts(File),
    NOpts = lists:usort(DefOpts ++ Opts ++ OptsInc),
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

include_dirs_opts(File) -> include_dirs_opts(File, []).
include_dirs_opts("/", Acc) -> Acc;
include_dirs_opts(".", Acc) -> Acc;
include_dirs_opts(File, Acc) ->
    Dir = filename:dirname(File),
    Acc1 = [{i, filename:join(Dir, "include")} | Acc],
    case filename:basename(File) of
        "src" -> Acc1;
        _ -> include_dirs_opts(Dir, Acc1)
    end.

