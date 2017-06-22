-module(async_server).

-behaviour(gen_server).

-export([
         start_link/0,
         heartbeat/0,
         event/1,
         watch/1,
         unwatch/1,
         pause/0,
         unpause/0,
         forget_changes/0
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
          refs = #{} :: #{string() => _Ref},
          patching = false :: boolean(),
          changed_files = queue:new() :: queue:queue(),
          timer :: timer:tref() | undefined,
          plugins :: any(),
          interval = 100 :: pos_integer()
         }).

%% API

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec heartbeat() -> ok.
heartbeat() -> gen_server:cast(?MODULE, heartbeat).

-spec event({File :: string(), Action :: atom()}) -> any().
event(Event = {_File, _Action}) -> ?MODULE ! {erlfsmon_events, Event}.

-spec watch(Path :: string()) -> ok | {error, _Reason}.
watch(Path) -> gen_server:call(?MODULE, {watch, Path}).
-spec unwatch(Path :: string()) -> ok | {error, _Reason}.
unwatch(Path) -> gen_server:call(?MODULE, {unwatch, Path}).

-spec pause() -> ok.
pause() -> gen_server:cast(?MODULE, pause).
-spec unpause() -> ok.
unpause() -> gen_server:call(?MODULE, unpause).
-spec forget_changes() -> ok.
forget_changes() -> gen_server:call(?MODULE, forget_changes).

%% Gen server callbacks
-spec init(_Args) -> {ok, #s{}}.
init(_Args) ->
    Plugins = async_plugin:init([]),
    % UserEvents = async_lib:env(events, []),
    % Events = [close_write, moved_to, move_self] ++ UserEvents,
    % UserOpts = async_lib:env(inotify_opts, []),
    % Opts = [recursive, {exclude, "\\.git*."}, {events, Events}] ++ UserOpts,

    UserPaths = async_lib:env(paths, []),
    ExcludePaths = [".", filename:join(code:root_dir(), "lib")],
    Refs = watch_path(code:get_path(), ExcludePaths, UserPaths),
    Interval = async_lib:env(collect_interval, 200), %% msec
    {ok, TRef} = timer:apply_interval(Interval, ?MODULE, heartbeat, []),
    {ok, #s{refs = Refs, plugins = Plugins,
            interval = Interval, timer = TRef}}.

-spec terminate(_Reason, #s{}) -> ok.
terminate(_Reason, #s{}) -> ok.

-spec code_change(_OldVsn, #s{}, _Extra) -> {ok, #s{}}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec handle_call(_Reqest, _From, #s{}) -> {noreply, #s{}}.
handle_call({watch, Path = [_ | _]}, _From, State = #s{refs = Refs}) ->
    case maps:find(Path, Refs) of
        {ok, _} ->
            {reply, {error, already_watch}, State};
        _ ->
            Refs1 = watch_path([Path]),
            {reply, ok, State#s{refs = maps:merge(Refs, Refs1)}}
    end;
handle_call({unwatch, Path = [_ | _]}, _From, State = #s{refs = Refs}) ->
    case maps:take(Path, Refs) of
        {Value, Refs2} ->
            erlfsmon:unsubscribe(Value),
            {reply, ok, State#s{refs = Refs2}};
        _ ->
            {reply, {error, not_found_subscibtion, State}}
    end;
handle_call(Reqest, _From, State) ->
    io:format("Unknown handle_call ~p ~n", [Reqest]),
    {noreply, State}.

-spec handle_cast(_Reqest, #s{}) -> {noreply, #s{}}.
handle_cast(heartbeat, State = #s{changed_files = Files,
                                  plugins = PlugStates}) ->
    ok = eval_changes(Files, PlugStates),
    {noreply, State#s{changed_files = queue:new()}};

handle_cast(pause, State = #s{timer = undefined}) ->
    {noreply, State};
handle_cast(unpause, State = #s{timer = Tref})
  when Tref /= undefined ->
    {noreply, State};
handle_cast(pause, State = #s{timer = TRef}) ->
    timer:cancel(TRef),
    {noreply, State};
handle_cast(unpause, State = #s{timer = undefined, interval = Interval}) ->
    {ok, TRef} = timer:apply_interval(Interval, ?MODULE, heartbeat, []),
    {noreply, State#s{timer = TRef}};
handle_cast(forget_changes, State = #s{}) ->
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

-spec watch_path(WatchPath :: [string()]) -> _Ref.
watch_path(WatchPath) ->
    watch_path(WatchPath, [], []).

-spec watch_path(Path, Path, Path) -> #{string() => _Ref}
                                        when Path :: [string()].
watch_path(WatchPath, ExcludePaths, UserPaths) ->
    PreSpyPaths = [filename:dirname(X) || X <- WatchPath -- ExcludePaths]
        ++ UserPaths,
    SpyPaths = lists:filtermap(
        fun(Dir) ->
            RealDir = async_lib:get_real_directory(Dir),
            DirSrc = filename:join(RealDir, "src"),
            EndPath = async_lib:get_real_directory(DirSrc),
            case filelib:is_dir(EndPath) of
                true -> {true, EndPath};
                false -> false
            end
        end, PreSpyPaths),

    case SpyPaths of
        [] -> #{};
        _ ->
            %% Add supplementary library for parse transform AST
            Pathsa = lists:usort([ filename:dirname(Dir) || Dir <- PreSpyPaths ]),
            code:add_pathsa(Pathsa),

            %% Follow for all directory in release
            Ref = erlfsmon:subscribe(SpyPaths, fun filter_all/1, [modified, renamed]),
            maps:from_list([ {Path, Ref} || Path <- SpyPaths ])
    end.

-spec filter_all(_) -> true.
filter_all(_) -> true.
