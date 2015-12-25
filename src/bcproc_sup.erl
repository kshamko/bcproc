%%%-------------------------------------------------------------------
%%% Description module bcproc_sup
%%%-------------------------------------------------------------------
%%% Main bcproc supervisor. Started by bcproc_app and responsible to
%%% start static supervisor for cleaning dead processes from broadcasters.
%%% Also it's responsible ta start broadcast pools and broadcast servers
%%% supervisors dynamically
%%%-------------------------------------------------------------------
-module(bcproc_sup).

-author("Konstantin Shamko").
-author_email("konstantin.shamko@gmail.com").

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  start_broadcaster/1,
  stop_broadcaster/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(BC_CHILD(I, Name, Type, Params), {{local, Name}, {I, start_link, Params}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%%----------------------------------------------------------------------
%% Function: start_link/1
%% Purpose: Starts the supervisor
%% Returns: A tuple {ok, Pid} or {error, Reason}
%%----------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Function: start_broadcaster/1
%% Purpose: Starts empty pool of broadcast servers and supervisor for broadcast servers
%%          and register these proceses.
%% Args:   Name - atom with a broadcaster name. Name is used to register broadcaster.
%% Returns: A tuple {ok, Pid} or {error, Reason}
%%----------------------------------------------------------------------
start_broadcaster(Name) ->
  SupName = bcproc_broadcast_sup:get_sup_name(Name),
  supervisor:start_child(?MODULE, ?BC_CHILD(bcproc_broadcast_sup, SupName, supervisor, [SupName])),
  supervisor:start_child(?MODULE, ?BC_CHILD(bcproc_bpool, Name, worker, [Name, SupName])).

%%----------------------------------------------------------------------
%% Function: stop_broadcaster/1
%% Purpose: Stops a broadcaster - a pool of broadcast servers and their supervisor and unregisters them.
%% Args:   Name - atom with a broadcaster name.
%% Returns: A tuple {ok, Pid} or {error, Reason}
%%----------------------------------------------------------------------
stop_broadcaster(Name) ->
  Result = supervisor:terminate_child(?MODULE, {local, Name}),
  supervisor:delete_child(?MODULE, {local, Name}),
  SupName = bcproc_broadcast_sup:get_sup_name(Name),
  supervisor:terminate_child(?MODULE, {local, SupName}),
  supervisor:delete_child(?MODULE, {local, SupName}),
  Result.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: supervisor callback
%% Returns: {ok, Spec}
%%----------------------------------------------------------------------
init([]) ->
  {ok, { {one_for_one, 5, 10}, [
    ?BC_CHILD(bcproc_cleaner_sup, bcproc_cleaner_sup, supervisor,[])
  ]} }.