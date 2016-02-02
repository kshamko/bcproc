%%%-------------------------------------------------------------------
%%% Description module bcproc_broadcast
%%%-------------------------------------------------------------------
%%% Actually this module processes contain clients pids to broadcast to
%%%-------------------------------------------------------------------
-module(bcproc_broadcast).

-author("Konstantin Shamko").
-author_email("konstantin.shamko@gmail.com").

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  broadcast/2,
  get_clients_count/1,
  add_client/2,
  terminate_client/2,
  set_clean_pids/3
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).
%%---------------------------------------------------------------------
%% Interval for cleanup of dead clients
%% TODO: move to application/release config
%%---------------------------------------------------------------------
-define(CLEANUP_TIME,  5 * 60 * 1000). %run clients cleanup every 5 minutes

%%---------------------------------------------------------------------
%% Data Type: state
%% where:
%%    clientsCount: count of clients added
%%    clients: dictionary of clients' pids
%%    poolServer: name of pool server
%%----------------------------------------------------------------------
-record(state, {clientsCount = 0, clients = dict:new(), poolServer}).

%%%===================================================================
%%% API
%%%===================================================================

%%----------------------------------------------------------------------
%% Function: start_link/2
%% Purpose: Starts broadcast server
%% Args:   PoolServerName - name of the pool process
%% Returns: {ok, Pid} or {error, Reason}
%%----------------------------------------------------------------------
start_link(PoolServerName) ->
  gen_server:start_link(?MODULE, [PoolServerName], []).

%%----------------------------------------------------------------------
%% Function: add_client/2
%% Purpose: Adds client (Pid or Name) to the broadcast server
%% Args:   Proc  - name or pid of broadcast server
%%         Client - pid or name of the process to sent broadcast message to
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%%----------------------------------------------------------------------
add_client(Proc, Client) ->
  gen_server:cast(Proc, {add_client, Client}).

%%----------------------------------------------------------------------
%% Function: terminate_client/2
%% Purpose: Removes client (Pid or Name) from the broadcast server
%% Args:   Proc  - name or pid of broadcast server
%%         Client - pid or name of the process to sent broadcast message to and which was
%%                  added to a broadcaster before
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%%----------------------------------------------------------------------
terminate_client(Proc, Client) ->
  gen_server:cast(Proc, {terminate_client, Client}).

%%----------------------------------------------------------------------
%% Function: broadcast/2
%% Purpose: Tells pool to broadcast message to all attached broadcast servers
%% Args:   Proc  - name or pid of broadcast server
%%         Msg - message to broadcast
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%%----------------------------------------------------------------------
broadcast(Proc, Msg) ->
  gen_server:cast(Proc, {broadcast, Msg}).

%%----------------------------------------------------------------------
%% Function: get_clients_count/1
%% Purpose: Returns count of clients added
%% Args:   Proc - name or pid of broadcast server
%% Returns: Count - integer
%%----------------------------------------------------------------------
get_clients_count(Proc) ->
  gen_server:call(Proc, get_count_clients).

%%----------------------------------------------------------------------
%% Function: set_clean_pids/3
%% Purpose: ReSet clients after cleanup
%% Args:   Proc - name or pid of broadcast server
%%         PidDict - dicrionary with clients
%%         PidCount - clients count
%% Returns: 'ok'
%%----------------------------------------------------------------------
set_clean_pids(Proc, PidDict, PidCount) ->
  gen_server:call(Proc, {set_clean_pids, PidDict, PidCount}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: gen_server callback. Also runs clients cleanup after ?CLEANUP_TIME interval
%% Returns: {ok, State}
%%----------------------------------------------------------------------
init([PoolServerName]) ->
  erlang:send_after(?CLEANUP_TIME, self(), cleanup),
  {ok, #state{poolServer = PoolServerName}}.

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: gen_server callback.
%%----------------------------------------------------------------------
%----------------------------------------------------------------------
% Returns count of clients attached to broadcast server
%----------------------------------------------------------------------
handle_call(get_count_clients, _From, State) ->
  {reply, State#state.clientsCount, State};
%----------------------------------------------------------------------
% Sets clients after cleanup. If count of clients is 0 broadcast server dies.
% But it's a so called current broadcast server it stays alive
%----------------------------------------------------------------------
handle_call({set_clean_pids, PidDict, 0}, _From, State) ->
  case process_info(self(), registered_name) of
    [] -> {stop, normal, ok, State};
    {registered_name, _Name} ->  {reply, ok, State#state{clients = PidDict, clientsCount = 0}}
  end;
%----------------------------------------------------------------------
% Sets clients after cleanup.
%----------------------------------------------------------------------
handle_call({set_clean_pids, PidDict, PidCount}, _From, State) ->
  {reply, ok, State#state{clients = PidDict, clientsCount = PidCount}};
%----------------------------------------------------------------------
% Catch other calls
%----------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%----------------------------------------------------------------------
%% Function: handle_cast/3
%% Purpose: gen_server callback.
%%----------------------------------------------------------------------
%----------------------------------------------------------------------
% Adds client
%----------------------------------------------------------------------
handle_cast({add_client, ClientPid}, State)->
  case dict:find(ClientPid, State#state.clients) of
    error ->
      ClientsCount = State#state.clientsCount + 1,
      Clients = dict:append(ClientPid, ClientPid, State#state.clients);
    _ ->
      ClientsCount = State#state.clientsCount,
      Clients = State#state.clients
  end,
  {noreply, State#state{clientsCount = ClientsCount, clients = Clients}};
%----------------------------------------------------------------------
% Removes client
%----------------------------------------------------------------------
handle_cast({terminate_client, ClientPid}, State) ->
  Clients = dict:erase(ClientPid, State#state.clients),
  ClientsCount = dict:size(Clients),
  {noreply, State#state{clientsCount = ClientsCount, clients = Clients}};
%----------------------------------------------------------------------
% Broadcast message
%----------------------------------------------------------------------
handle_cast({broadcast, Msg}, State) ->
  Clients = State#state.clients,
  dict:map(
    fun(WsPid, _Value) when is_pid(WsPid) ->
      WsPid ! {bcast, Msg}
    end,
    Clients),
  {noreply, State};
%----------------------------------------------------------------------
% Catch other calls
%----------------------------------------------------------------------
handle_cast(_Request, State) ->
  {noreply, State}.

%%----------------------------------------------------------------------
%% Function: handle_info/2
%% Purpose: gen_server callback. %% Handling all non call/cast messages
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%%--------------------------------------------------------------------
%----------------------------------------------------------------------
% Performs cleanup of dead clients
%----------------------------------------------------------------------
handle_info(cleanup, State) ->
  Clients = State#state.clients,
  {ok, CleanerPid} = bcproc_cleaner_sup:start_cleaner(),
  bcproc_cleaner_server:clean(CleanerPid, Clients, self()),
  erlang:send_after(?CLEANUP_TIME, self(), cleanup),
  {noreply, State};
%----------------------------------------------------------------------
% Catch other calls
%----------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.


%%--------------------------------------------------------------------
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @spec terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(normal, _State) ->
  normal;
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Convert process state when code is changed
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.