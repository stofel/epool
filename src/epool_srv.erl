%%%----------------------------------------------------------------------
%%% File    : epool_srv.erl
%%% Author  : Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%% Purpose : Pool manage server
%%% Created : 25 Mar 2014 by Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%%----------------------------------------------------------------------

-module(epool_srv).
-behaviour(gen_server).

%
-define(debug, true).
-include_lib("epool.hrl").

%
-export([start/1, start_link/1, handle_info/2, code_change/3, terminate/2]).
-export([init/1, handle_call/3, handle_cast/2, stop/1]).
-export([req_sync/4]).

%% DEFINES
-define(WORKERS_SUP, epool_workers_sup).

%% The friendly supervisor is started dynamically!
-define(WORKERS_SUP_SPEC(Pool),
        {list_to_atom(atom_to_list(Pool#pool.name) ++ "_workers_sup"),
        {epool_workers_sup, start_link, [Pool]},
         temporary,
         10000,
         supervisor,
        [epool_workers_sup]}).
        
%%
-define(WORKER_SPEC(Module, Func, Args),
  {worker,
  {Module, Func, [Args]},
   temporary,
   10000,
   worker,
  [Module]}).



% Start Pool
start(Pool = #pool{name = Name, workers = Workers}) when is_atom(Name), is_integer(Workers) ->
  gen_server:start({local, Name}, ?MODULE, Pool, []).

start_link(Pool = #pool{name = Name, workers = Workers}) when is_atom(Name), is_integer(Workers) ->
  Result = gen_server:start_link({local, Name}, ?MODULE, Pool, []),
  %?debugf(start_link, {Result, Pool}),
  Result.


% Stop Pool
stop(Name) ->
  gen_server:call(Name, stop).


%% Gen server
init(Pool = #pool{workers = Workers}) ->
    self() ! start_worker_supervisor,
    init_workers(Workers),
    {ok, Pool}.

%
init_workers(0) -> ok;
init_workers(N) ->
  self() ! start_worker,
  init_workers(N-1).


%% CALLs
handle_call(stop, _From, Pool) -> {stop, normal, ok, Pool};
handle_call(get_state, _From, Pool) -> {reply, {ok, Pool}, Pool};
handle_call(get_workers_pids, _From, Pool = #pool{workers_sup = Sup}) -> 
  % {undefined,<0.56.0>,worker,[worker]}
  Childs = [P || {_,P,_,_} <- supervisor:which_children(Sup)],
  {reply, {ok, Childs}, Pool};
handle_call(_Msg, _From, Pool) -> {noreply, Pool}.


%% CASTs
handle_cast({add_tasks, cast, TaskList}, Pool = #pool{tasks = {BTasks, ATask}}) ->
  Length = length(TaskList),
  NewTask = {cast, gb_sets:from_list(lists:zip(lists:seq(1, Length), TaskList))},
  NewPool = force_tasks(Pool#pool{tasks = {[NewTask|BTasks], ATask}}),
  {noreply, NewPool};

%
handle_cast({add_tasks, Pid, TaskList}, Pool = #pool{tasks = {BTasks, ATask}, answers = Answers}) ->
  Length = length(TaskList),
  NewTask = {Pid, gb_sets:from_list(lists:zip(lists:seq(1, Length), TaskList))},
  NewAnswer = #answer{length = Length},
  %?debugf("Pool1", Pool),
  NewPool = 
    force_tasks(
      Pool#pool{tasks   = {[NewTask|BTasks], ATask}, 
                answers = gb_trees:enter(Pid, NewAnswer, Answers)}),
  %?debugf("Pool2", NewPool),
  {noreply, NewPool};
  

handle_cast(force_tasks, Pool) -> 
  NewPool = force_tasks(Pool),
  {noreply, NewPool};

%
handle_cast({resize, Workers}, Pool = #pool{name = PoolName, workers_sup = Sup}) -> 
  Pids = [P || {_,P,_,_} <- supervisor:which_children(Sup)],
  case Workers - length(Pids) of
    Diff when Diff > 0 -> 
      init_workers(Diff),
      gen_server:cast(PoolName, force_tasks),
      {noreply, Pool#pool{workers = Workers}};
    Diff when Diff < 0 -> 
      [exit(Pid, stop_due_resize) || Pid <- lists:nthtail(Workers, Pids)],
      {noreply, Pool#pool{workers = Workers}};
    _ -> 
      {noreply, Pool}
  end;

% 
handle_cast(_Msg, State) -> {noreply, State}.


%
handle_info({{ok, get_worker_answer}, Pid, {cast, _, _}}, Pool = #pool{free = Free}) ->
        NewPool = force_tasks(Pool#pool{free = gb_sets:add(Pid, Free)}),
        {noreply, NewPool};
handle_info({{killed, get_worker_answer}, _Pid, {cast, _, _}}, Pool) ->
        NewPool = force_tasks(Pool),
        {noreply, NewPool};
handle_info({{Satus, get_worker_answer}, Pid, {FromKey, TaskKey, Result}}, 
             Pool = #pool{answers = Answers, free = Free}) ->
    TasksAnswers = gb_trees:get(FromKey, Answers),
    NewTasksAnswers = gb_sets:add({TaskKey, Result}, TasksAnswers#answer.answers),
    NewTasksAnswersLength = gb_sets:size(NewTasksAnswers),
    NewFree = 
      case Satus of
        ok -> gb_sets:add(Pid, Free);
        killed -> Free
      end,
    if 
      NewTasksAnswersLength >= TasksAnswers#answer.length ->
        % return result
        PoolAnswer = [X || {_, X} <- gb_sets:to_list(NewTasksAnswers)],
        FromKey ! {pool_answer, PoolAnswer},
        NewAnswers = gb_trees:delete(FromKey, Answers),
        NewPool = force_tasks(Pool#pool{answers = NewAnswers, free = NewFree}),
        {noreply, NewPool};
      true ->
        NewAnswers = gb_trees:enter(FromKey, TasksAnswers#answer{answers = NewTasksAnswers}, Answers),
        NewPool = force_tasks(Pool#pool{answers = NewAnswers, free = NewFree}),
        {noreply, NewPool}
    end;
%
handle_info(start_worker, Pool = #pool{workers_sup = Sup, free = Free, refs = Refs}) ->
    {ok, Pid} = supervisor:start_child(Sup, []),
    NewRef = erlang:monitor(process, Pid),
    NewRefs = gb_sets:insert(NewRef, Refs),
    {noreply, Pool#pool{free = gb_sets:add(Pid, Free), refs = NewRefs}};
handle_info(start_worker_supervisor, Pool = #pool{pool_sup = Sup}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?WORKERS_SUP_SPEC(Pool)),
    {noreply, Pool#pool{workers_sup = Pid}};
%
handle_info({'DOWN', Ref, process, OldPid, stop_due_resize}, Pool = #pool{refs = Refs, free = Free}) ->
    case gb_sets:is_element(Ref, Refs) of
      true ->
        NewRefs = gb_sets:delete_any(Ref, Refs),
        NewFree = gb_sets:delete_any(OldPid, Free),
        {noreply, Pool#pool{refs=NewRefs, free = NewFree}};
      false -> %% Not our responsibility
        {noreply, Pool}
    end;
%     
handle_info({'DOWN', Ref, process, OldPid, shutdown}, Pool = #pool{refs = Refs, free = Free}) ->
  NewRefs = gb_sets:delete_any(Ref, Refs),
  NewFree = gb_sets:delete_any(OldPid, Free),
  {noreply, Pool#pool{refs=NewRefs, free = NewFree}};
%
handle_info({'DOWN', Ref, process, OldPid, _Msg}, 
            Pool = #pool{workers_sup  = Sup, 
                         refs         = Refs, 
                         free         = Free, 
                         errors       = Errors}) ->
    case gb_sets:is_element(Ref, Refs) of
      true ->
        {ok, NewPid} = supervisor:start_child(Sup, []),
        NewRef = erlang:monitor(process, NewPid),
        NewRefs = gb_sets:insert(NewRef, gb_sets:delete_any(Ref, Refs)),
        NewFree = gb_sets:insert(NewPid, gb_sets:delete_any(OldPid, Free)),
        NewPool = force_tasks(Pool#pool{refs=NewRefs, free = NewFree, errors = Errors + 1}),
        {noreply, NewPool};
      false -> %% Not our responsibility
        {noreply, Pool}
    end;

handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.








%%Check free workers and fill it by tasks
force_tasks(Pool = #pool{free = {0,nil}}) -> 
  Pool;
force_tasks(Pool = #pool{tasks = {[], []}}) ->
  Pool;
force_tasks(Pool = #pool{tasks = {[], ATasks}}) ->
  force_tasks(Pool#pool{tasks = {ATasks, []}});
force_tasks(Pool = #pool{tasks = {[{FromKey, Tasks}|BTasks], ATasks}, free = Free, task_ttl = TimeOut}) ->
  {Pid, NewFree} = gb_sets:take_smallest(Free),
  %?debugf("Tasks", Tasks),
  case gb_sets:take_smallest(Tasks) of
    {{TaskKey, Task}, {0,nil}} ->
      spawn(?MODULE, req_sync, [{FromKey, TaskKey, Task}, Pid, self(), TimeOut]),
      force_tasks(Pool#pool{tasks = {BTasks, ATasks}, free = NewFree});
    {{TaskKey, Task}, NewTasks} ->
      spawn(?MODULE, req_sync, [{FromKey, TaskKey, Task}, Pid, self(), TimeOut]),
      force_tasks(Pool#pool{tasks = {BTasks, [{FromKey, NewTasks}|ATasks]}, free = NewFree})
  end.


%% request task
req_sync({FromKey, TaskKey, Task}, Pid, From, TimeOut) ->
  {Status, Answer} =
    try gen_server:call(Pid, Task, TimeOut) of
      Result -> {ok, Result}
    catch
      exit:{timeout, _} -> 
        exit(Pid, kill),
        {killed, worker_timeout};
      ElseClass:ElseTerm ->
        exit(Pid, kill),
        {killed, {worker_error, {ElseClass, ElseTerm}}}
    end,
  From ! {{Status, get_worker_answer}, Pid, {FromKey, TaskKey, Answer}},
  ok.

