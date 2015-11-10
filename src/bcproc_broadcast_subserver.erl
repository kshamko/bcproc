%%%-------------------------------------------------------------------
%%% @author konstantin.shamko
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 27. Oct 2015 1:22 PM
%%%-------------------------------------------------------------------
-module(bcproc_broadcast_subserver).
-author("konstantin.shamko").

-behaviour(gen_server).

%% API
-export([start_link/1, broadcast/2, get_clients_count/1, add_client/2, terminate_client/2, set_clean_pids/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(CLEANUP_TIME,  30 * 1000). %run tokens cleanup every 30 seconds

-record(state, {clientsCount = 0, clients = dict:new(), server}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(ServerName) ->
  gen_server:start_link(?MODULE, [ServerName], []).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
get_clients_count(ProcName) ->
  gen_server:call(ProcName, get_count_clients).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
add_client(ProcName, ClientPid) ->
  gen_server:cast(ProcName, {add_client, ClientPid}).


%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate_client(ProcName, ClientPid) ->
  gen_server:cast(ProcName, {terminate_client, ClientPid}).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
broadcast(ProcName, Msg) ->
  gen_server:cast(ProcName, {broadcast, Msg}).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
set_clean_pids(SubserverPid, PidDict, PidCount) ->
  gen_server:call(SubserverPid, {set_clean_pids, PidDict, PidCount}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ServerName]) ->
  erlang:send_after(?CLEANUP_TIME, self(), cleanup),
  {ok, #state{server = ServerName}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(get_count_clients, _From, State) ->
  {reply, State#state.clientsCount, State};
handle_call({set_clean_pids, PidDict, 0}, _From, State) ->
  case process_info(self(), registered_name) of
    [] ->
      bcproc_broadcast_server:remove_subserver(State#state.server, self()),
      {stop, normal, ok, State};
    {registered_name, _Name} ->
      {reply, ok, State#state{clients = PidDict, clientsCount = 0}}
  end;
handle_call({set_clean_pids, PidDict, PidCount}, _From, State) ->
      {reply, ok, State#state{clients = PidDict, clientsCount = PidCount}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
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

handle_cast({terminate_client, ClientPid}, State) ->
  Clients = dict:erase(ClientPid, State#state.clients),
  ClientsCount = dict:size(Clients),
  {noreply, State#state{clientsCount = ClientsCount, clients = Clients}};

handle_cast({broadcast, Msg}, State) ->
  Clients = State#state.clients,
  dict:map(
    fun(WsPid, _Value) when is_pid(WsPid) ->
      WsPid ! {text, Msg}
    end,
    Clients),

  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(cleanup, State) ->
  Clients = State#state.clients,
  {ok, CleanerPid} = bcproc_cleaner_sup:start_cleaner(),
  bcproc_cleaner_server:clean(CleanerPid, Clients, self()),
  erlang:send_after(?CLEANUP_TIME, self(), cleanup),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(normal, _State) ->
  normal;
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
