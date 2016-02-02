%%%-------------------------------------------------------------------
%%% Description module bcproc_broadcast_sup
%%%-------------------------------------------------------------------
%%% Supervisor for broadcast servers
%%%-------------------------------------------------------------------
-module(bcproc_broadcast_sup).

-author("Konstantin Shamko").
-author_email("konstantin.shamko@gmail.com").

-behaviour(supervisor).

%% API
-export([
  start_link/1,
  start_bcserver/2,
  get_sup_name/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%----------------------------------------------------------------------
%% Function: start_link/1
%% Purpose: Starts the supervisor
%% Args:   SupName - supervisor name to start with
%% Returns: A tuple {ok, Pid} or {error, Reason}
%%----------------------------------------------------------------------
start_link(SupName) ->
  supervisor:start_link({local, SupName}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Function: start_bcserver/2
%% Purpose: Starts broadcast server inside of pool with PoolServerName
%% Args:   SupervisorName - atom with a broadcast servers' supervisor name.
%%         PoolServerName - atom with a pool server name.
%% Returns: A tuple {ok, Pid} or {error, Reason}
%%----------------------------------------------------------------------
start_bcserver(SupervisorName, PoolServerName) ->
  supervisor:start_child(SupervisorName, [PoolServerName]).


%%----------------------------------------------------------------------
%% Function: get_sup_name/1
%% Purpose: Generates name of supervisor based on pool name
%% Args:   PoolName - atom with a pool name.
%% Returns: An atom
%%----------------------------------------------------------------------
get_sup_name(PoolName) ->
  list_to_atom(lists:concat([PoolName] ++ ['_sup'])).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
  MaxRestarts = 5,
  MaxTime = 240,
  {ok, {{simple_one_for_one, MaxRestarts, MaxTime}, [
    {bcproc_broadcast, {bcproc_broadcast, start_link, []}, transient, 2000, worker, [bcproc_broadcast]}
  ]}}.