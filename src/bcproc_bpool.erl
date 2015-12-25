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
%%----------------------------------------------------------------------
-record(state, {serverName, broadcastSup}).

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
%%         Client - pid or name of the process to sent broadcast message to
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
  start_bcserver(BcSupName, ServerName),
  {ok, #state{ serverName = ServerName, broadcastSup = BcSupName}}.

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
% ToDo: optimize this because to remove one client it's required to loop through
%       all broadcast servers and clients attached to them
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
%% Returns: Pid of newly started broadcast server
%%----------------------------------------------------------------------
start_bcserver(SupervisorName, PoolServerName) ->
  LatestName = latest_name(SupervisorName),
  {ok, BcPid} = bcproc_broadcast_sup:start_subserver(SupervisorName, PoolServerName),
  register_latest_subserver(BcPid, LatestName, whereis(LatestName)),
  BcPid.

%%----------------------------------------------------------------------
%% Function: broadcast/2
%% Purpose: Tells pool to broadcast nessage to all binded broadcast servers
%% Args:   ServerName - pool process name.
%%         Client - pid or name of the process to sent broadcast message to
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%%----------------------------------------------------------------------
register_latest_subserver(LatestPid, Name, undefined) ->
  register(Name, LatestPid);
register_latest_subserver(LatestPid, Name, _OldPid) ->
  unregister(Name),
  register_latest_subserver(LatestPid, Name, undefined).


%%--------------------------------------------------------------------
%% @todo add configuration param for processes count threshold
%% @private
%% @doc
%% Returns active subserver to add clients to.
%%
%% @end
%%--------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Function: broadcast/2
%% Purpose: Tells pool to broadcast nessage to all binded broadcast servers
%% Args:   ServerName - pool process name.
%%         Client - pid or name of the process to sent broadcast message to
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%% ToDo:
%%----------------------------------------------------------------------
get_current_bcserver(State) ->
  CurPid = whereis(latest_name(State#state.broadcastSup)),

  CountSubservers = bcproc_broadcast:get_clients_count(CurPid),
  case CountSubservers >= ?PROCESSES_TO_BCSERVER of
    true ->
      start_bcserver(State#state.broadcastSup, State#state.serverName);
    _ -> CurPid
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Register latest started subserver under a name
%%
%% @end
%%--------------------------------------------------------------------
latest_name(SupevisorName) ->
  list_to_atom(lists:concat([SupevisorName] ++ ['_latest_child'])).

