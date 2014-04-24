%%%----------------------------------------------------------------------
%%% File    : epool_example.erl
%%% Author  : Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%% Purpose : epool test example api
%%% Created : 07 Apr 2014 by Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%%----------------------------------------------------------------------

-module(epool_example).

%
-define(debug, true).
-include_lib("epool.hrl").
 
-compile(export_all).

-define(TEST_POOL_NAME, test_pool).

%% Test wraps
start() ->
  Options = [
    {workers,  4},      % Number of workers
    {task_ttl, 1500}],  % (ms) Task ttl, if worker not return answer in this time 
                        % worker_timeout returned and worker killed by exit(Pid(), kill)
  MFA = {epool_example_worker, start_link, []},
  epool:start_pool(?TEST_POOL_NAME, MFA, Options).


%
stop() ->
  epool:stop_pool(?TEST_POOL_NAME).

%
cast() ->
  Tasks = [{ping, N} || N <- lists:seq(1,100)],
  cast(Tasks).
cast(Tasks) -> epool:cast(?TEST_POOL_NAME, Tasks).
%

call() ->
  Tasks = [{ping, 1}, {ping, 2}],
  TimeOut = 100000, % (ms) Time to wait receive answer from epool, 
                    % If the server is just late with the reply, 
                    % it may arrive at any time later into the 
                    % caller's message queue. 
                    % The caller must in this case be prepared 
                    %  for this and discard any such garbage.
  call(Tasks, TimeOut).

call(Tasks, TimeOut) -> epool:call(?TEST_POOL_NAME, Tasks, TimeOut).

%
childs() ->
  gen_server:call(?TEST_POOL_NAME, get_workers_pids).

