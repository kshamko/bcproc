
-module(bcproc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_server/1, stop_server/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(BC_CHILD(I, Name, Type, Params), {{local, Name}, {I, start_link, Params}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_server(Name) ->
  SupName = bcproc_broadcast_sup:get_sup_name(Name),
  {ok, _} = supervisor:start_child(?MODULE, ?BC_CHILD(bcproc_broadcast_sup, SupName, supervisor, [SupName])),
  {ok, _} = supervisor:start_child(?MODULE, ?BC_CHILD(bcproc_broadcast_server, Name, worker, [Name, SupName])).

stop_server(Name) ->
  SupName = bcproc_broadcast_sup:get_sup_name(Name),
  supervisor:terminate_child(?MODULE, {local, Name}),
  supervisor:delete_child(?MODULE, {local, Name}),
  supervisor:terminate_child(?MODULE, {local, SupName}),
  supervisor:delete_child(?MODULE, {local, SupName}).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_one, 5, 10}, [
    ?BC_CHILD(bcproc_cleaner_sup, bcproc_cleaner_sup, supervisor,[])
  ]} }.


%% ===================================================================
%% Private functions
%% ===================================================================



