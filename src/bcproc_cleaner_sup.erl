%%%-------------------------------------------------------------------
%%% Description module bcproc_broadcast_sup
%%%-------------------------------------------------------------------
%%% Supervisor for clients cleaner
%%%-------------------------------------------------------------------
-module(bcproc_cleaner_sup).

-author("Konstantin Shamko").
-author_email("konstantin.shamko@gmail.com").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1, start_cleaner/0]).

-define(SUPERVISOR, ?MODULE).

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
  supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts cleaner server
%%
%% @end
%%--------------------------------------------------------------------
start_cleaner() ->
  supervisor:start_child(?SUPERVISOR, []).

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
      {bcproc_cleaner_server, {bcproc_cleaner_server, start_link, []}, transient, 2000, worker, [bcproc_cleaner_server]}
    ]}}.
