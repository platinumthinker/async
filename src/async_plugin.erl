-module(async_plugin).

-type filetype()   :: string().
-type opt()        :: {atom(), any()}.
-type filename()   :: file:filename().
-type plugin()     :: module().
-type event()      :: atom().
-type out()        :: [string()].
-type module_bin() :: binary().

-export([
         init/1,
         chain/1
        ]).

-export_type([
              filetype/0,
              filename/0,
              plugin/0,
              opt/0,
              event/0,
              out/0,
              module_bin/0
             ]).

-record(s, {
          plugins   = #{}  :: #{ plugin() => _PluginState },
          filetypes = #{}  :: #{ filetype() => plugin() },
          ext       = ""   :: filetype(),
          file      = ""   :: filename(),
          module    = none :: module()
         }).

%% Supported filetypes for processed current plugin
-callback filetypes() -> [filetype()].
-callback init(_Opts) -> _State.
-callback terminate(_State) -> any().

%% Run when file changed and current plugin support this filetype
-callback change({filetype(), File :: filename(), _Event :: atom()}, _State) ->
    %% Call function `compile` for all files return from this callback
    {ok, FileChanges :: {filetype(), filename(), [opt()]} } |
    {error, _Reason} |
    done.

%% Processed event queue
-callback compile({filetype(), filename(), [opt()]}, _State) ->
    %% Compile but not loading
    ok |
    %% Compile and return binary for loading code
    {ok, {module(), binary(), Out} } |
    %% Compile and return only output for information (compile err\warn\info)
    {ok | done | error,  Out} |
    %% Don't compile and return {error, warnings}
    {error, {Out, Out}} when Out :: [string()].

%% Run before code load
-callback pre_load({filename(), module(), Binary :: binary()}, _State) ->
    ok | done | {error, _Reason}.

%% Run after code load
-callback after_load({module()}, _State) -> ok.

%% ================================ API ======================================
-spec init(_Opts) -> #s{}.
init(_) ->
    {Plugins, Files} = fold(
        fun(Plugin, {AccState, AccFiletype}) ->
                NewAcc = lists:foldl(
                    fun(FType, Acc) ->
                            Acc#{FType => Plugin}
                    end, AccFiletype, Plugin:filetypes()),
                { AccState#{Plugin => Plugin:init([])}, NewAcc }
        end, {#{}, #{}}),
    #s{plugins = Plugins, filetypes = Files}.

-spec chain({{Path:: string(), Events :: [atom()]}, #s{}}) ->
    ok | {ok | error, _Reason}.
chain({{Path, Event}, State}) ->
    Chain = [
             fun change/1,
             fun compile/1,
             fun pre_load/1,
             fun load_binary/1,
             fun after_load/1
            ],
    File = filename:basename(Path),
    Dir = filename:dirname(Path),
    user_callback('init', {Event, File, Dir}),
    Res = case async_lib:chain(Chain, {Event, File, Dir, State}) of
        {error, Reason} ->
            user_callback('error', {File, Reason}),
            {error, Reason};
        Other -> Other
    end,
    user_callback('end', {Event, File, Dir}),
    Res.

%% ========================== Plugin Action Chain ============================
%% Callback Change file  -> done | error
%%     v ok
%% Callback Compile file -> done | error
%%     v ok
%% Callback pre_load     -> done | error
%%     v ok
%% Loading module        -> error
%%     v ok
%% Callback after_load

change({Event, File, Dir, State}) ->
    Ext = case filename:extension(File) of
        []   -> [];
        Ext1 -> string:substr(Ext1, 2)
    end,
    FilePath = filename:absname(filename:join(Dir, File)),
    Args = {Ext, FilePath, Event},
    case plug(Ext, change, Args, State) of
        nothing ->
            {done, {not_found_plugin_for, Ext}};
        {ok, {NFile, NOpts}} ->
            NExt = filename:extension(NFile),
            NState = State#s{ext = NExt, file = NFile},
            {ok, {{NExt, NFile, NOpts}, NState}};
        {ok, Opts} ->
            {ok, {{Ext, FilePath, Opts}, State#s{ext = Ext, file = FilePath} }};
        %% Error or Done
        {Other, Any} -> {Other, {Any, State}}
    end.

compile({Arg, State = #s{ext = Ext} }) ->
    case plug(Ext, compile, Arg, State) of
        {ok, Module, Binary, []} ->
            {ok, {Module, Binary, [], State}};
        {ok, Module, Binary, Warnings} ->
            async_lib:format_list(Warnings, "WARNINGS"),
            {ok, {Module, Binary, Warnings, State}};
        {error, Errors, _Warnings}      ->
            async_lib:format_list(Errors, "ERRORS"),
            {error, compile_with_error};
        %% Error or Done
        {Other, Any} -> {Other, {Any, State}}
    end.

pre_load({Module, Binary, Warn, State = #s{ext = Ext} }) ->
    NState = State#s{module = Module},
    case plug(Ext, pre_load, {Module, Binary, Warn}, State) of
        ok -> {ok, {Binary, NState}};
        %% Error or Done
        {Other, Any} -> {Other, {Any, NState}}
    end.

load_binary({Binary, State = #s{file = File, module = Module}}) ->
    case code:load_binary(Module, File, Binary) of
        {module, Module} ->
            user_callback(load, Module),
            {ok, State};
        Err -> Err
    end.

after_load(State = #s{ext = Ext, module = Module}) ->
    plug(Ext, after_load, {Ext, Module}, State),
    {ok, State}.

%% ======================= Internal helper functions ==========================
plugins() ->
    [ async_erl, async_hrl, async_dtl ] ++ async_lib:env(plugins, []).

fold(Fun, Acc) -> lists:foldl(Fun, Acc, plugins()).

plug(Ext, Action, Arg, #s{plugins = Plugins, filetypes = FS}) ->
    Res = case maps:find(Ext, FS) of
        {ok, Plugin} ->
            #{ Plugin := PluginState } = Plugins,
            Plugin:Action(Arg, PluginState);
        _ ->
            nothing
    end,
    user_callback(Action, Arg),
    Res.

-spec user_callback(Func :: atom(), Arg :: tuple() | atom()) -> ok.
user_callback(Func, Arg) ->
    case async_lib:env(Func, []) of
        MF = {_, _} -> run_user_callback([MF], Func, Arg);
        Funcs  -> run_user_callback(Funcs, Func, Arg)
    end.

-spec run_user_callback([{M :: atom(), F :: atom()}], Func :: atom(), _) -> ok.
run_user_callback([{cmd, Command = [_Char | _]} | T], Func, Arg) ->
    os:cmd(io_lib:format("'~ts' '~p' '~p'", [Command, Func, Arg])),
    run_user_callback(T, Func, Arg);
run_user_callback([{M, F} | T], Func, Arg)
  when is_atom(M), is_atom(F) ->
    case erlang:function_exported(M, F, 1) of
        true ->
            catch M:F(Arg);
        _ ->
            ok
    end,
    run_user_callback(T, Func, Arg);
run_user_callback([_ | T], Func, Arg) -> run_user_callback(T, Func, Arg);
run_user_callback([], _, _) -> ok.
