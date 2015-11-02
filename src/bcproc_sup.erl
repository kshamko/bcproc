
-module(bcproc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_server/1, stop_server/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(BC_CHILD(I, Type, Name), {Name, {I, start_link, [Name]}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_server(Name) ->
  supervisor:start_child(?MODULE, ?BC_CHILD(bcproc_broadcast_sup, supervisor, [bcproc_broadcast_sup:get_sup_name(Name)])),
  supervisor:start_child(?MODULE, ?BC_CHILD(bcproc_broadcast_server, worker, [Name])).

stop_server(Name) ->
  supervisor:terminate_child(?MODULE, get_sup_name(Name)),
  supervisor:terminate_child(?MODULE, Name).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_one, 5, 10}, []} }.


%% ===================================================================
%% Private functions
%% ===================================================================



