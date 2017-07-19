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
unpause() -> gen_server:cast(?MODULE, unpause).
-spec forget_changes() -> ok.
forget_changes() -> gen_server:cast(?MODULE, forget_changes).

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
    Refs = watch_path(code:get_path() ++ UserPaths, ExcludePaths),
    Interval = async_lib:env(collect_interval, 200), %% msec
    {ok, TRef} = case async_lib:env(pause, false) of
        false ->
            timer:apply_interval(Interval, ?MODULE, heartbeat, []);
        true ->
            {ok, undefined}
    end,
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
%% Apply changes by timer
handle_cast(heartbeat, State = #s{changed_files = Files,
                                  plugins = PlugStates}) ->
    ok = eval_changes(Files, PlugStates),
    {noreply, State#s{changed_files = queue:new()}};

%% Cancel timer for pause
handle_cast(pause, State = #s{timer = TRef})
  when TRef =/= undefined ->
    timer:cancel(TRef),
    {noreply, State#s{timer = undefined}};
%% Add timer for unpause
handle_cast(unpause, State = #s{timer = undefined, interval = Interval}) ->
    {ok, TRef} = timer:apply_interval(Interval, ?MODULE, heartbeat, []),
    {noreply, State#s{timer = TRef}};
%% Already pause
handle_cast(pause, State = #s{timer = undefined}) ->
    {noreply, State};
%% Already unpause
handle_cast(unpause, State = #s{}) ->
    {noreply, State};

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
    watch_path(WatchPath, []).

-spec watch_path(Path, Path) -> #{string() => _Ref}
                                        when Path :: [string()].
watch_path(WatchPath, ExcludePaths) ->
    PreSpyPaths = [filename:dirname(X) || X <- WatchPath -- ExcludePaths],
    SpyPaths = lists:foldl(
        fun(Dir, AccRes) ->
            RealDir = async_lib:get_real_directory(Dir),
            Dirs = lists:filtermap(
                fun(Dir1) ->
                    Path = filename:join(RealDir, Dir1),
                    EndPath = async_lib:get_real_directory(Path),
                    filelib:is_dir(EndPath) andalso {true, EndPath}
                end, ["src", "include", "priv"]),
            Dirs ++ AccRes
        end, [], PreSpyPaths),

    RegExpPaths = async_lib:env(exclude_path_regexp, []),
    FilterSpyPath = lists:filter(
        fun(Dir) ->
            not lists:any(
                fun(RegExp) ->
                    case re:run(Dir, RegExp, [unicode, {capture, first}]) of
                        {match, _} -> true;
                        _ -> false
                    end
                end, RegExpPaths)
        end, SpyPaths),


    io:format("Monitoring ~p~n", [FilterSpyPath]),

    case SpyPaths of
        [] -> #{};
        _ ->
            %% Add supplementary library for parse transform AST
            Pathsa = lists:usort([ filename:dirname(Dir) || Dir <- PreSpyPaths ]),
            code:add_pathsa(Pathsa),

            %% Follow for all directory in release
            Ref = erlfsmon:subscribe(FilterSpyPath, fun filter_all/1, [modified, renamed]),
            maps:from_list([ {Path, Ref} || Path <- FilterSpyPath])
    end.

-spec filter_all(_) -> true.
filter_all(_) -> true.
