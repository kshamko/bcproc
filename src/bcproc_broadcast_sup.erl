%%%-------------------------------------------------------------------
%%% @author konstantin.shamko
%%% @copyright (C) 2015, Oxagile LLC
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2015 2:30 PM
%%%-------------------------------------------------------------------
-module(bcproc_broadcast_sup).
-author("konstantin.shamko").

-behaviour(supervisor).

%% API
-export([start_link/1, start_subserver/2, get_sup_name/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link(SupName) ->
  supervisor:start_link({local, SupName}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_subserver(SupervisorName, ServerName) ->
  supervisor:start_child(SupervisorName, [ServerName]).


%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
get_sup_name(ServerName) ->
  list_to_atom(lists:concat([ServerName] ++ ['_sup'])).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
  MaxRestarts = 5,
  MaxTime = 240,
  {ok, {{simple_one_for_one, MaxRestarts, MaxTime}, [
    {bcproc_broadcast, {bcproc_broadcast, start_link, []}, transient, 2000, worker, [bcproc_broadcast]}
  ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

