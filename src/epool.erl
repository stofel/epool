%%%----------------------------------------------------------------------
%%% File    : epool.erl
%%% Author  : Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%% Purpose : epool api
%%% Created : 25 Mar 2014 by Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%%----------------------------------------------------------------------

-module(epool).

%
-define(debug, true).
-include_lib("epool.hrl").


%% App exports
-export([start_link/0, stop/0]).

%% Pool export
-export([
    start_pool/2, start_pool/3,
    stop_pool/1,
    state_pool/1, 
    call/2, call/3, cast/2,
    size/1, resize/2,
    status_pool/1, % For testing and monitoring, not for production
    list/0 % For testing and monitoring, not for production
  ]).



%%
start_link() ->
  epool_supersup:start_link().
%%
stop() ->
  epool_supersup:stop().

%%
-spec start_pool(atom(), mfa()) -> {ok, pid()} | {error, Reason::term()}.
start_pool(PoolName, MFA) ->
  Options = [
    {workers,  4},      % Number of workers
    {task_ttl, 1500}],  % (ms) Task ttl, if worker not return answer in this time 
                        % worker_timeout returned and worker killed by exit(Pid(), kill)
  start_pool(PoolName, MFA, Options).
-spec start_pool(atom(), mfa(), list()) -> {ok, pid()} | {error, Reason::term()}.
start_pool(PoolName, MFA, Options) -> 
  Pool = #pool{
    name = PoolName,
    mfa = MFA,
    workers = proplists:get_value(workers, Options, #pool{}#pool.workers),
    task_ttl = proplists:get_value(task_ttl, Options, #pool{}#pool.task_ttl)},
  epool_supersup:start_pool(Pool).

%%
-spec stop_pool(atom()) -> ok | {error, Reason::term()}.
stop_pool(PoolName) -> 
  %{ok, Pool} = state_pool(PoolName),
  epool_supersup:stop_pool(PoolName).

%%
-spec list() -> list().
list() ->
  [PoolName || {PoolName, _Pid, _Type, _Modules} <- supervisor:which_children(epool)].

%%
-spec state_pool(atom()) -> list() | term() | atom().
state_pool(PoolName) ->
  case lists:member(PoolName, list()) of
    true -> gen_server:call(PoolName, get_state);
    false -> no_such_pool
  end.

  


%%
-spec call(atom(), list()) -> {ok, list()} | atom() | {error, Reason::term()}.
call(PoolName, TasksList) ->
  call(PoolName, TasksList, ?DEFAULT_TASKS_TTL).
%%
-spec call(atom(), list(), integer()) -> {ok, list()} | atom() | {error, Reason::term()}.
call(PoolName, TasksList, Timeout) ->
  case lists:member(PoolName, list()) of
    true ->
      SelfKey = self(), % Tasks Key for multiple running tasks
      gen_server:cast(PoolName, {add_tasks, SelfKey, TasksList}),
      receive
        {pool_answer, Answers} -> 
          {ok, Answers};
        Else -> 
          {error, {unknown_pool_answer, Else}}
      after 
        Timeout -> pool_timeout
      end;
    _ -> no_such_pool
  end.


%%
-spec cast(atom(), list()) -> ok.
cast(PoolName, TasksList) ->
  SelfKey = cast,
  gen_server:cast(PoolName, {add_tasks, SelfKey, TasksList}),
  ok.


%% Pool Status
-spec status_pool(atom()) -> {ok, list()} | atom() | {error, Reason::term()}.
status_pool(PoolName) ->
  case state_pool(PoolName) of
   {ok, PoolState} ->
      {BTasks, ATasks} = PoolState#pool.tasks,
      Tasks = lists:append(BTasks, ATasks),
      SumTasksLength = 
        lists:sum([gb_sets:size(T) || {_P, T} <- Tasks]),
      SumAnswersLength = 
        lists:sum([gb_sets:size(T) || {_P, T} <- gb_trees:to_list(PoolState#pool.answers)]),
      {ok,
        [{tasks, length(Tasks)},
         {sum_task_length, SumTasksLength},
         {sum_answers_length, SumAnswersLength},
         {size, gb_sets:size(PoolState#pool.refs)},
         {free_workers, gb_sets:size(PoolState#pool.free)},
         {errors, PoolState#pool.errors}]};
   Else -> Else
  end.
 
%%
-spec size(atom()) -> {ok, integer()} | atom() | {error, Reason::term()}.
size(PoolName) ->
  case state_pool(PoolName) of
    {ok, Pool} ->
      {ok, Pool#pool.workers};
    Else -> Else
  end.

%%
-spec resize(atom(), integer()) -> ok | atom() | {error, Reason::term()}.
resize(PoolName, Workers) when Workers > 0 ->
  gen_server:cast(PoolName, {resize, Workers}).

