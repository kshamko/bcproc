%%%-------------------------------------------------------------------
%%% Description module bcproc_bpool
%%%-------------------------------------------------------------------
%%% Implementation of pool of broadcast servers. Also it's an interface for
%%% broadcast server (bcproc_broadcast).
%%%-------------------------------------------------------------------
-module(bcproc_bpool).

-author("Konstantin Shamko").
-author_email("konstantin.shamko@gmail.com").

-behaviour(gen_server).

%% API
-export([
  start_link/2,
  add_client/2,
  remove_client/2,
  broadcast/2
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%%--------------------------------------------------------------------
%% defines how many clients can be registered inside of broadcast server.
%% if limit reached new broadcast server started by pool
%%--------------------------------------------------------------------
-define(PROCESSES_TO_BCSERVER, 250).

%%---------------------------------------------------------------------
%% Data Type: state
%% where:
%%    serverName: name of the pool process
%%    broadcastSup: Name of the broadcast servers' supervisor
%%    currentBc: name of current broadcast server
%%----------------------------------------------------------------------
-record(state, {serverName, broadcastSup, currentBc}).

%%%===================================================================
%%% API
%%%===================================================================

%%----------------------------------------------------------------------
%% Function: start_link/2
%% Purpose: Starts pool
%% Args:   ServerName - name of the pool process
%%         BcSupName - Name of the broadcast servers' supervisor
%% Returns: {ok, Pid} or {error, Reason}
%%----------------------------------------------------------------------
start_link(ServerName, BcSupName) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [ServerName, BcSupName], []).

%%----------------------------------------------------------------------
%% Function: add_client/2
%% Purpose: Adds client (Pid or Name) to the pool with ServerName.
%% Args:   ServerName - pool process name.
%%         Client - pid or name of the process to sent broadcast message to
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%%----------------------------------------------------------------------
add_client(ServerName, Client) ->
  gen_server:cast(ServerName, {add_client, Client}).

%%----------------------------------------------------------------------
%% Function: remove_client/2
%% Purpose: Removes client (Pid or Name) from a pool with ServerName.
%% Args:   ServerName - pool process name.
%%         Client - pid or name of the process to sent broadcast message to and which was
%%                  added to a broadcaster before
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%%----------------------------------------------------------------------
remove_client(ServerName, ClientPid) ->
  gen_server:cast(ServerName, {remove_client, ClientPid}).

%%----------------------------------------------------------------------
%% Function: broadcast/2
%% Purpose: Tells pool to broadcast message to all attached broadcast servers
%% Args:   ServerName - pool process name.
%%         Msg - message to broadcast
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%%----------------------------------------------------------------------
broadcast(ServerName, Msg) ->
  gen_server:cast(ServerName, {broadcast, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: gen_server callback
%% Returns: {ok, State}
%%----------------------------------------------------------------------
init([ServerName, BcSupName]) ->
  LatestBcName = latest_bcname(BcSupName),
  start_bcserver(BcSupName, ServerName, LatestBcName),
  {ok, #state{ serverName = ServerName, broadcastSup = BcSupName, currentBc = LatestBcName}}.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: gen_serve callback
%%----------------------------------------------------------------------
%---------------------------------------
% Handle add client. Locates current broadcast server and binds client to it
%---------------------------------------
handle_cast({add_client, ClientPid}, State) ->
  BcPid = get_current_bcserver(State),
  bcproc_broadcast:add_client(BcPid, ClientPid),
  {noreply, State};
%---------------------------------------
% Handle remove client. Notifies all attached broadcast servers to remove client
%---------------------------------------
handle_cast({remove_client, ClientPid}, State) ->
  BcServers = supervisor:which_children(State#state.broadcastSup),
  [ bcproc_broadcast:terminate_client(BcServer, ClientPid) || {_,BcServer,_,_} <- BcServers],
  {noreply, State};
%%---------------------------------------
%% Handle broadcast. Broadcasts message to all broadcast servers attached
%%---------------------------------------
handle_cast({broadcast, Msg}, State) ->
  BcServers = supervisor:which_children(State#state.broadcastSup),
  [ bcproc_broadcast:broadcast(BcServer, Msg) || {_,BcServer,_,_} <- BcServers],
  {noreply, State};
%%---------------------------------------
%% Catch all other cast messages
%%---------------------------------------
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%----------------------------------------------------------------------
%% Function: start_bcserver/2
%% Purpose: Start broadcast server
%% Args:   PoolServerName - pool process name.
%%         SupervisorName - name of supervisor for broadcast servers
%%         LatestBcName - name of current broadcast server
%% Returns: Pid of newly started broadcast server
%%----------------------------------------------------------------------
start_bcserver(SupervisorName, PoolServerName, LatestBcName) ->
  {ok, BcPid} = bcproc_broadcast_sup:start_bcserver(SupervisorName, PoolServerName),
  register_latest_bcserver(BcPid, LatestBcName, whereis(LatestBcName)),
  BcPid.

%%----------------------------------------------------------------------
%% Function: register_latest_bcserver/3
%% Purpose: Register newly created broadcast server under a Name.
%% Args:   LatestPid - pid of newly create broadcast server
%%         Name - name to register LatestPid under
%%         OldPid - pid or 'undefined' atom - pid of previous broadcast server
%% Returns: An atom 'true'
%%----------------------------------------------------------------------
register_latest_bcserver(LatestPid, Name, undefined) ->
  register(Name, LatestPid);
register_latest_bcserver(LatestPid, Name, _OldPid) ->
  unregister(Name),
  register_latest_bcserver(LatestPid, Name, undefined).


%%----------------------------------------------------------------------
%% Function: get_current_bcserver/1
%% Purpose: Returns latest broadcast server to deal with
%% Args:   State - state of pool process
%% Returns: pid of latest broadcast server
%% TODO: add configuration param for processes count threshold
%%----------------------------------------------------------------------
get_current_bcserver(State) ->
  CurPid = whereis(State#state.currentBc),

  CountBcServers = bcproc_broadcast:get_clients_count(CurPid),
  case CountBcServers >= ?PROCESSES_TO_BCSERVER of
    true ->
      start_bcserver(State#state.broadcastSup, State#state.serverName, State#state.currentBc);
    _ -> CurPid
  end.

%%----------------------------------------------------------------------
%% Function: latest_bcname/1
%% Purpose: Generates name for latest broadcast server to register it under
%% Args:   SupervisorName - name of broadcast servers' supervisor
%% Returns: An atom with name
%%----------------------------------------------------------------------
latest_bcname(SupervisorName) ->
  list_to_atom(lists:concat([SupervisorName] ++ ['_latest_child'])).

