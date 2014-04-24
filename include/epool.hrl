%%%----------------------------------------------------------------------
%%% File    : epool.hrl
%%% Author  : Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%% Purpose : epool include file
%%% Created : 25 Mar 2014 by Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%%----------------------------------------------------------------------

%%
%% DEFAULTS
%%
-define(DEFAULT_TASKS_TTL, 600000). % 10 Min for task list process timeout


%%
%% records
%% 

%% Used for start pool and for handle state of started pool
-record(pool, {
    from                ::pid(),            % tasks src process pid 
    name                ::atom(),           % Pool name atom
    workers = 4         ::integer(),        % Pool workers num
    mfa,                                    % {Module, Function, Args} to start worker
    errors  = 0         ::integer(),        % workers crashes num 
    pool_sup            ::pid(),            % Pid of pool supervisor
    workers_sup         ::pid(),            % Pid of workers supervisor
    tasks = {[], []},                       % lists tasks to workers, for round robin algoritm
    task_ttl = 1000     ::integer(),        % (ms) ttl to task process
    answers = gb_trees:empty(),             % list workers answers gb_sets
    free = gb_sets:new() ::[pid()],          % list of pids free workers
    %busy                ::[pid()]           % list of pids busy workers
    refs = gb_sets:new()                    % list of workers references (for supervisor)
  }).
  
%
-record(answer, {
    answers = gb_sets:new(), % gb_sets of answers
    length  ::integer()   % length of tasks list
  }).



%%
%% Debug macros
%%
-ifdef(debug).
-define(debugf(Str, X),  io:format("Mod:~w line:~w ~p ~100P~n", [?MODULE,?LINE, Str, X, 30])).
-define(debugff(Str, X), io:format("Mod:~w line:~w ~p~n ~100P~n", [?MODULE,?LINE, Str, X, 100])).
-else.
-define(debugf(X, Y), true).
-define(debugff(X, Y), true).
-endif.

