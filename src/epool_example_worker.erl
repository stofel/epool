%%%----------------------------------------------------------------------
%%% File    : epool_example_worker.erl
%%% Author  : Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%% Purpose : epool example worker module
%%% Created : 28 Mar 2014 by Aleksey Kluchnikov <kluchnikovas@gmail.com>
%%%----------------------------------------------------------------------


%%
-module(epool_example_worker).
-behaviour(gen_server).

%%
-define(debug, true).
-include_lib("epool.hrl").


%%
-export([start_link/1, init/1, handle_call/3, 
         handle_cast/2, handle_info/2, code_change/3, 
         terminate/2]).

 

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  {ok, Args}.


%%% OTP Callbacks
handle_call({ping, Key}, _From, State) ->
  Response = ping(Key),
  {reply, Response, State};
%%
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
%
handle_call(_Msg, _From, State) ->
  {noreply, State}.

%%
handle_cast(_Msg, State) ->
  {noreply, State}.
%
handle_info(_Msg, State) ->
  {noreply, State}.
 
code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.
terminate(_Reason, _State) -> 
  ok.



%%%%% Worker functions %%%%%
ping(Key) ->
  {A,B,C} = now(),
  random:seed(A,B,C),
  SleepTime = random:uniform(5),
  if SleepTime == 1 -> SleepTime = 2; true -> do_nothing end, % crash some times
  timer:sleep(SleepTime * 100),
  {pong, Key}.

