-module(async_dtl).

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
filetypes() -> [ "dtl" ].

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
change({"dtl", _File, _Event}, #s{}) ->
    {ok, [report, return, binary,
          debug_info, {auto_escape,false}]}.

-spec compile({async_plugin:filetype(),
              async_plugin:filename(),
              [async_plugin:opt()]}, #s{}) ->
    {ok, {module(), async_plugin:filename(), ModBin :: binary(),
          Warn :: [string()]}} |
    {error, {Err :: [string()], Warn :: [string()]}}.
compile({"dtl", File, Opts}, _State) ->
    Module = lists:flatten(filename:basename(File, ".dtl") ++ "_view"),
    case erlydtl:compile(File, list_to_atom(Module), Opts) of
        %% Module not changed
        {ok, _ModuleRet, <<>>, _Warn} -> {done, not_changed};
        {ok, _ModuleRet, _Binary, _Warn} = Ret ->
            Ret;
        Error ->
            {error, Error}
    end.

-spec pre_load(_, #s{}) -> ok.
pre_load({_, _, _}, _) -> ok.

-spec after_load(_, #s{}) -> ok.
after_load(_, _) -> ok.
