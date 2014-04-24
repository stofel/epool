%%%----------------------------------------------------------------------
%%% File    : epool_supersup.erl
%%% Author  : Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%% Purpose : Pools supervisor
%%% Created : 25 Mar 2014 by Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%%----------------------------------------------------------------------

%%
%% Idea from:
%% http://learnyousomeerlang.com/building-applications-with-otp
%%

-module(epool_supersup).
-behaviour(supervisor).

%
-define(debug, true).
-include_lib("epool.hrl").


%% API
-export([start_link/0, stop/0, start_pool/1, stop_pool/1]).
-export([init/1]).

%% DEFINES
-define(POOL_SUP, epool_sup).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, epool}, ?MODULE, []).

%% technically, a supervisor can not be killed in an easy way.
%% Let's do it brutally!
stop() ->
  case whereis(?MODULE) of
    P when is_pid(P) ->
      exit(P, kill);
    _ -> ok
  end.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

% What is the top level supervisor exactly? 
% Well its only task is to hold pools in memory and supervise them. 
% In this case, it will be a childless supervisor:
init([]) -> {ok, {{one_for_one, 1, 100}, []}}.



%% ===================================================================
%% Module API
%% ===================================================================

%
start_pool(Pool = #pool{name = Name}) ->
  ChildSpec = 
    {Name,
      {?POOL_SUP, start_link, [Pool]},
       temporary, 10500, supervisor, [?POOL_SUP]},
  Result = supervisor:start_child(epool, ChildSpec),
  Result.

%
stop_pool(Name) ->
  supervisor:terminate_child(epool, Name).

