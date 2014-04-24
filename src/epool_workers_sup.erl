%%%----------------------------------------------------------------------
%%% File    : epool_workers_sup.erl
%%% Author  : Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%% Purpose : Pool workers supervisor
%%% Created : 25 Mar 2014 by Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%%----------------------------------------------------------------------


-module(epool_workers_sup).
-behaviour(supervisor).

%
-define(debug, true).
-include_lib("epool.hrl").

%% API
-export([start_link/1]).
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

%
start_link(Pool) ->
  supervisor:start_link(?MODULE, Pool).
    
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%
init(#pool{mfa = {Module, Func, Args}}) ->
  MaxRestart = 1,
  MaxTime = 60,
  %[{worker, {M,F,A}, temporary, 5000, worker, [M]}]
  WorkerTpl = {worker, 
    {Module, Func, [Args]},
    temporary, 10000, worker,
    [Module]},

  {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [WorkerTpl]}}.


