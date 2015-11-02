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
-export([start_link/1, add_client/2, remove_client/2, broadcast/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(PROCESSES_TO_SUBSERVER, 250).

-record(state, {serverName, currentSubserver, subserversCount = 0, subservers = []}).

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
  gen_server:start_link({local, ServerName}, ?MODULE, [], []).

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


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([ServerName]) ->
  {ok, #state{ serverName = ServerName}}.

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
  {SubCount, {SubName, Subservers}} = get_current_subserver(State),
  bcproc_broadcast_subserver:add_client(SubName, ClientPid),
  {noreply, State#state{currentSubserver = SubName, subserversCount = SubCount, subservers = Subservers}};

%%---------------------------------------
%% Handle remove client
%%---------------------------------------
handle_cast({remove_client, ClientPid}, State) ->
  Subservers = State#state.subservers,
  [ bcproc_broadcast_subserver:terminate_client(Subserver, ClientPid) || Subserver <- Subservers],
  {noreply, State};

%%---------------------------------------
%% Handle broadcast
%%---------------------------------------
handle_cast({broadcast, Msg}, State) ->
  Subservers = State#state.subservers,
  [ bcproc_broadcast_subserver:broadcast(Subserver, Msg) || Subserver <- Subservers],
  {noreply, State};

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
%% @todo add configuration param for processes count threshold
%% @doc
%% Function takes curent process state and starts new subserver if required
%% Desision is based on ?PROCESSES_TO_SUBSERVER constant's value
%% @end
%%--------------------------------------------------------------------
start_subserver(State) ->
  Count = list_to_binary(integer_to_list(State#state.subserversCount)),
  SubserverName = binary_to_atom(<<"broadcast_subserver_", Count/binary>>, utf8),
  {ok, _} = bcproc_broadcast_sup:start_subserver(State#state.serverName, SubserverName),
  Subservers = State#state.subservers ++ [SubserverName],
  {SubserverName, Subservers}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns active subserver to add clients to.
%%
%% @end
%%--------------------------------------------------------------------
get_current_subserver(State) ->
  case State#state.currentSubserver of
    undefined -> {State#state.subserversCount + 1, start_subserver(State)};
    _->
      case bcproc_broadcast_subserver:get_clients_count(State#state.currentSubserver) >= ?PROCESSES_TO_SUBSERVER of
        true -> {State#state.subserversCount + 1, start_subserver(State)};
        _ -> {State#state.subserversCount, {State#state.currentSubserver, State#state.subservers}}
      end
  end.


