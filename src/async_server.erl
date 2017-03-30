-module(async_server).

-behaviour(gen_server).

-include_lib("systools/include/inotify.hrl").

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
          inotify :: pid(),
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

-spec event(#inotify{} | {File :: string(), Event :: atom()}) -> any().
event(Inotify = #inotify{}) -> ?MODULE ! {from_api, Inotify};
event({File, Event}) ->
    F = filename:basename(File),
    Dir = filename:dirname(File),
    Inotify = #inotify{file = F, event = Event, watched = Dir, isdir = false},
    event(Inotify).

%% Gen server callbacks
-spec init(_Args) -> #s{}.
init(_Args) ->
    Plugins = async_plugin:init([]),
    UserEvents = async_lib:env(events, []),
    Events = [close_write, moved_to, move_self] ++ UserEvents,
    UserOpts = async_lib:env(inotify_opts, []),
    Opts = [recursive, {exclude, "\\.git*."}, {events, Events}] ++ UserOpts,

    UserPaths = async_lib:env(paths, []),
    ExcludePaths = [".", filename:join(code:root_dir(), "lib")],
    SpyPaths = [filename:dirname(X) || X <- code:get_path() -- ExcludePaths]
        ++ UserPaths,

    %% Add supplementary library for parse transform AST
    code:add_pathsa(lists:usort([ filename:dirname(Dir) || Dir <- SpyPaths ])),

    %% Follow for all directory in release
    Inotify = inotifywait:start(string:join(SpyPaths, " "), Opts),
    Interval = async_lib:env(collect_interval, 600), %% msec
    {ok, TRef} = timer:apply_interval(Interval, ?MODULE, heartbeat, []),
    {ok, #s{inotify = Inotify, timer = TRef, plugins = Plugins}}.

-spec terminate(_Reason, #s{}) -> ok.
terminate(_Reason, #s{inotify = Inotify}) ->
    inotifywait:close(Inotify),
    ok.

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
handle_info({Id, File = #inotify{}},
            State = #s{inotify = Inotify, changed_files = Files})
  when Id == Inotify; Id == from_api ->
    {noreply, State#s{changed_files = queue:in(File, Files)}};

handle_info(Info, State) ->
    io:format("Unknown handle_info ~p ~n", [Info]),
    {noreply, State}.

%% ====================== Internal functions ================================
-spec eval_changes(queue:queue(#inotify{}), any()) -> ok.
eval_changes(Files, PlugStates) ->
    case queue:out(Files) of
        {empty, _} -> ok;
        {{value, Ev = #inotify{file = File, watched = Dir}}, Files1} ->
            Path = filename:absname(filename:join(Dir, File)),
            async_pool:spawn(Path, fun async_plugin:chain/1, {Ev, PlugStates}),
            eval_changes(Files1, PlugStates)
    end.
