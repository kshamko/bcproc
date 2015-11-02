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
-export([start_link/0, start_subserver/1]).

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
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%-------------------------------
%%
%%-------------------------------
start_subserver(SubserverName) ->
  Subserver = {{local, SubserverName}, {screen_broadcast_subserver, start_link, [SubserverName]}, transient, 2000, worker, [screen_broadcast_subserver]},
  supervisor:start_child(?SERVER, Subserver).

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
  {ok, {{one_for_one, MaxRestarts, MaxTime}, [

  ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

