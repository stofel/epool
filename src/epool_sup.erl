%%%----------------------------------------------------------------------
%%% File    : epool_sup.erl
%%% Author  : Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%% Purpose : Pool supervisor
%%% Created : 25 Mar 2014 by Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%%----------------------------------------------------------------------

%%
-module(epool_sup).
-behaviour(supervisor).

%%
-define(debug, true).
-include_lib("epool.hrl").

%% 
-export([start_link/1]).
-export([init/1]).

%%
-define(POOL_SRV, epool_srv).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Pool) ->
  supervisor:start_link(?MODULE, Pool).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Pool = #pool{name = Name}) ->
  RegName = list_to_atom(atom_to_list(Name) ++ "_pool_sup"),
  MaxRestart = 1,
  MaxTime = 5,
  ChildSpec = 
    {RegName, 
    {?POOL_SRV, start_link, [Pool#pool{pool_sup= self()}]}, 
     permanent, 
     5000, 
     worker, 
    [?POOL_SRV]},
  %?debugf("start_sup", ChildSpec),
  {ok, {
      {one_for_all, MaxRestart, MaxTime},
      [ChildSpec]
    }
  }.


