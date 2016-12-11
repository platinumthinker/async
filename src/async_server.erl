-module(async_server).

-behaviour(gen_server).

-include_lib("systools/include/inotify.hrl").

-export([
         start_link/0,
         heartbeat/0
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
          timer :: timer:tref() | undefined
         }).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API
-spec heartbeat() -> ok.
heartbeat() -> gen_server:cast(?MODULE, heartbeat).

-spec init(_Args) -> #s{}.
init(_Args) ->
    Events = [close_write, moved_to, move_self],
    Opts = [recursive, {exclude, "\\.git*."}, {events, Events}],
    Inotify = inotifywait:start(".", Opts),
    Interval = 2000, %% msec
    {ok, TRef} = timer:apply_interval(Interval, ?MODULE, heartbeat, []),
    {ok, #s{inotify = Inotify, timer = TRef}}.

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
handle_cast(heartbeat, State = #s{changed_files = Files}) ->
    io:format("heartbeat ~p ~n", [Files]),
    {noreply, State#s{changed_files = queue:new()}};
handle_cast(_Reqest, State = #s{changed_files = {[], []}}) ->
    io:format("empty ~n"),
    {noreply, State};
handle_cast(Reqest, State) ->
    io:format("Unknown handle_cast ~p ~n", [Reqest]),
    {noreply, State}.

-spec handle_info(_Info, #s{}) -> {noreply, #s{}}.
handle_info({Id, File = #inotify{}},
            State = #s{inotify = Inotify, changed_files = Files})
  when Id == Inotify ->
    io:format("Add ~p~n", [File]),
    {noreply, State#s{changed_files = queue:in(File, Files)}};

handle_info(Info, State) ->
    io:format("Unknown handle_info ~p ~n", [Info]),
    {noreply, State}.
