-module(async_pool).

-behaviour(gen_server).

-include_lib("systools/include/inotify.hrl").

-export([
         start_link/1
        ]).

-export([ spawn/2 ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-type pool() :: pid() | atom().
-type pool_id() :: term().

-export_type([pool/0]).

-record(s, {
          pool_limit = 1 :: pos_integer(),
          current_workers = 0 :: non_neg_integer(),
          child :: {pid(), erlang:references()} | undefined,
          sched_queue :: queue:queue({Id :: pool_id(), fun()}) | undefined
         }).

-spec start_link(Max :: pos_integer()) -> {ok, Limiter :: pool()}.
start_link(Max) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Max], []).

-spec spawn(Id :: any(), Fun :: fun(() -> any())) -> ok.
spawn(Id, Fun) -> gen_server:cast(?MODULE, {spawn, Id, Fun}).

-spec init(Max :: pos_integer()) -> {ok, #s{}}.
init(_Max) ->
    {ok, #s{pool_limit = 1,
            sched_queue = queue:new()}}.

-spec handle_cast(_Request, #s{}) -> {noreply, #s{}}.
handle_cast({spawn, Id, Fun}, State) ->
    NewState = worker_spawn(Id, Fun, State),
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_call(_Request, _From, #s{}) -> {noreply, #s{}}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_info(any(), #s{}) -> {noreply, #s{}}.
%% Handle monitor for workers
handle_info({'DOWN', _, _, _, _}, State = #s{sched_queue = Queue,
                                                 current_workers = Current}) ->
    {Queue1, NewChild, Current1} = worker_eval(Queue, Current - 1),
    {noreply, State#s{current_workers = Current1, child = NewChild,
                      sched_queue = Queue1}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_Reason, #s{}) -> ok.
terminate(_Reason, #s{child = {Pid, MonRef}}) ->
    %% Clean all works
    erlang:demonitor(MonRef),
    erlang:exit(Pid, killed),
    ok.

-spec code_change(_OldVsn, #s{}, _Extra) -> {ok, #s{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

worker_spawn(Id, Fun, State = #s{pool_limit = Limit,
                                 current_workers = Current,
                                 child           = Child,
                                 sched_queue     = Queue}) ->
    Queue1 = queue:in({Id, Fun}, Queue),
    {NewQueue, NewChild, Current1} = case Current < Limit of
        true  -> worker_eval(Queue1, Current);
        false -> {Queue1, Child, Current}
    end,
    State#s{current_workers = Current1,
            sched_queue     = NewQueue,
            child           = NewChild}.

worker_eval(Queue, Current) ->
    case queue:out(Queue) of
        {empty, Queue1} -> {Queue1, undefined, Current};
        {{value, {_Id1, Fun1}}, Queue1} ->
            PidRef = erlang:spawn_monitor(Fun1),
            {Queue1, PidRef, Current + 1}
    end.
