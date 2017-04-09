-module(async_server).

-behaviour(gen_server).

-export([
         start_link/0,
         heartbeat/0,
         event/1
        ]).

-export([
         init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3
        ]).

-record(s, {
          ref :: any(),
          pause = false :: boolean(),
          patching = false :: boolean(),
          changed_files = queue:new() :: queue:queue(),
          timer :: timer:tref() | undefined,
          plugins :: any()
         }).

%% API

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec heartbeat() -> ok.
heartbeat() -> gen_server:cast(?MODULE, heartbeat).

-spec event({File :: string(), Action :: atom()}) -> any().
event(Event = {_File, _Action}) -> ?MODULE ! {erlfsmon_events, Event}.

%% Gen server callbacks
-spec init(_Args) -> #s{}.
init(_Args) ->
    Plugins = async_plugin:init([]),
    % UserEvents = async_lib:env(events, []),
    % Events = [close_write, moved_to, move_self] ++ UserEvents,
    % UserOpts = async_lib:env(inotify_opts, []),
    % Opts = [recursive, {exclude, "\\.git*."}, {events, Events}] ++ UserOpts,

    UserPaths = async_lib:env(paths, []),
    ExcludePaths = [".", filename:join(code:root_dir(), "lib")],
    PreSpyPaths = [filename:dirname(X) || X <- code:get_path() -- ExcludePaths]
        ++ UserPaths,
    SpyPaths = lists:map(
        fun(Dir) ->
            RealDir = async_lib:get_real_directory(Dir),
            async_lib:get_real_directory(filename:join(RealDir, "src"))
        end, PreSpyPaths),
    %% Add supplementary library for parse transform AST
    code:add_pathsa(lists:usort([ filename:dirname(Dir) || Dir <- PreSpyPaths ])),

    %% Follow for all directory in release
    Ref = erlfsmon:subscribe(SpyPaths, fun(_) -> true end, [modified, renamed]),
    Interval = async_lib:env(collect_interval, 200), %% msec
    {ok, TRef} = timer:apply_interval(Interval, ?MODULE, heartbeat, []),
    {ok, #s{ref = Ref, timer = TRef, plugins = Plugins}}.

-spec terminate(_Reason, #s{}) -> ok.
terminate(_Reason, #s{}) -> ok.

-spec code_change(_OldVsn, #s{}, _Extra) -> {ok, #s{}}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec handle_call(_Reqest, _From, #s{}) -> {noreply, #s{}}.
handle_call(Reqest, _From, State) ->
    io:format("Unknown handle_call ~p ~n", [Reqest]),
    {noreply, State}.

-spec handle_cast(_Reqest, #s{}) -> {noreply, #s{}}.
handle_cast(heartbeat, State = #s{changed_files = Files,
                                  plugins = PlugStates}) ->
    ok = eval_changes(Files, PlugStates),
    {noreply, State#s{changed_files = queue:new()}};
handle_cast(Reqest, State) ->
    io:format("Unknown handle_cast ~p ~n", [Reqest]),
    {noreply, State}.

-spec handle_info(_Info, #s{}) -> {noreply, #s{}}.
handle_info({erlfsmon_events, {File, Actions}},
            State = #s{changed_files = Files}) ->
    {noreply, State#s{changed_files = queue:in({File, Actions}, Files)}};

handle_info(Info, State) ->
    io:format("Unknown handle_info ~p ~n", [Info]),
    {noreply, State}.

%% ====================== Internal functions ================================
-spec eval_changes(queue:queue(file:filename()), any()) -> ok.
eval_changes(Files, PlugStates) ->
    case queue:out(Files) of
        {empty, _} -> ok;
        {{value, Event = {File, _Actions}}, Files1} ->
            async_pool:spawn(File, fun async_plugin:chain/1, {Event, PlugStates}),
            eval_changes(Files1, PlugStates)
    end.
