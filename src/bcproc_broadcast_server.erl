%%%-------------------------------------------------------------------
%%% @author konstantin.shamko
%%% @copyright (C) 2015, Oxagile LLC
%%% @doc
%%%
%%% @end
%%% Created : 27. Oct 2015 12:47 PM
%%%-------------------------------------------------------------------
-module(bcproc_broadcast_server).
-author("konstantin.shamko").

-behaviour(gen_server).

%% API
-export([start_link/2, add_client/2, remove_client/2, broadcast/2, remove_subserver/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(PROCESSES_TO_SUBSERVER, 250).

-record(state, {serverName, subserverSup, subservers}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(ServerName, SubserversSupName) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [ServerName, SubserversSupName], []).

%%-----------------------
%% @doc
%% Add client to brodcast messages to
%%
%% @end
%%-----------------------
add_client(ServerName, ClientPid) ->
  case is_pid(ClientPid) of
    true -> gen_server:cast(ServerName, {add_client, ClientPid});
    _ -> {error, "No Pid provided"}
  end.

%%-----------------------
%% @doc
%% Remove client from broadcast server
%%
%% @end
%%-----------------------
remove_client(ServerName, ClientPid) ->
  gen_server:cast(ServerName, {remove_client, ClientPid}).

%%-----------------------
%% @doc
%% Make a broadcast
%%
%% @end
%%-----------------------
broadcast(ServerName, Msg) ->
  gen_server:cast(ServerName, {broadcast, Msg}).

%%-----------------------
%% @doc
%% Remove subserver
%%
%% @end
%%-----------------------
remove_subserver(ServerName, SubserverPid) ->
  gen_server:cast(ServerName, {remove_subserver, SubserverPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([ServerName, SubserversSupName ]) ->
  {_, Subservers} = start_subserver(SubserversSupName, ServerName),
  {ok, #state{ serverName = ServerName, subserverSup = SubserversSupName, subservers = Subservers}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

%%---------------------------------------
%% Handele add client
%%---------------------------------------
handle_cast({add_client, ClientPid}, State) ->
  {SubserverPid, Subservers} = get_current_subserver(State),
  bcproc_broadcast_subserver:add_client(SubserverPid, ClientPid),
  {noreply, State#state{subservers = Subservers}};

%%---------------------------------------
%% Handle remove client
%%---------------------------------------
handle_cast({remove_client, ClientPid}, State) ->
  Subservers = supervisor:which_children(State#state.subserverSup),
  [ bcproc_broadcast_subserver:terminate_client(Subserver, ClientPid) || {_,Subserver,_,_} <- Subservers],
  {noreply, State};

%%---------------------------------------
%% Handle broadcast
%%---------------------------------------
handle_cast({broadcast, Msg}, State) ->
  Subservers = supervisor:which_children(State#state.subserverSup),
  [ bcproc_broadcast_subserver:broadcast(Subserver, Msg) || {_,Subserver,_,_} <- Subservers],
  {noreply, State};
%%---------------------------------------
%% remove subserver
%%---------------------------------------
handle_cast({remove_subserver, SubserverPid}, State) ->
  Subservers = lists:delete(SubserverPid, State#state.subservers),
  {noreply, State#state{subservers = Subservers}};
%%---------------------------------------
%% Catch all other cast messages
%%---------------------------------------
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.


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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function takes curent process state and starts new subserver if required
%% Desision is based on ?PROCESSES_TO_SUBSERVER constant's value
%% @end
%%--------------------------------------------------------------------
start_subserver(SupevisorName, ServerName) ->
  LatestName = latest_name(SupevisorName),
  {ok, SubserverPid} = bcproc_broadcast_sup:start_subserver(SupevisorName, ServerName),
  register_latest_subserver(SubserverPid, LatestName, whereis(LatestName)),
  {SubserverPid, supervisor:which_children(SupevisorName)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Register latest started subserver under a name
%%
%% @end
%%--------------------------------------------------------------------
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
get_current_subserver(State) ->
  CurPid = whereis(latest_name(State#state.subserverSup)),

  CountSubservers = bcproc_broadcast_subserver:get_clients_count(CurPid),
  case CountSubservers >= ?PROCESSES_TO_SUBSERVER of
    true ->
      start_subserver(State#state.subserverSup, State#state.serverName);
    _ -> {CurPid, State#state.subservers}
  end.

latest_name(SupevisorName) ->
  list_to_atom(lists:concat([SupevisorName] ++ ['_latest_child'])).

