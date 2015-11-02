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
-export([start_link/0, add_client/1, terminate_client/1, broadcast/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(PROCESSES_TO_SUBSERVER, 250).

-record(state, {currentSubserver, subserversCount = 0, subservers = []}).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%-----------------------
%
%-----------------------
add_client(Pid) ->
  case is_pid(Pid) of
    true -> gen_server:cast(?SERVER, {add_client, Pid});
    _ -> {error, "No Pid provided"}
  end.

terminate_client(Pid) ->
  gen_server:cast(?SERVER, {terminate_client, Pid}).

broadcast(Msg) ->
  gen_server:cast(?SERVER, {broadcast, Msg}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({add_client, ClientPid}, State) ->
  {SubCount, {SubName, Subservers}} = get_current_subserver(State),
  screen_broadcast_subserver:add_client(SubName, ClientPid),
  {noreply, State#state{currentSubserver = SubName, subserversCount = SubCount, subservers = Subservers}};
handle_cast({terminate_client, ClientPid}, State) ->
  Subservers = State#state.subservers,
  [ screen_broadcast_subserver:terminate_client(Subserver, ClientPid) || Subserver <- Subservers],
  {noreply, State};
handle_cast({broadcast, Msg}, State) ->
  lager:log(info, self(), "Broadcast started"),
  Subservers = State#state.subservers,
  [ screen_broadcast_subserver:broadcast(Subserver, Msg) || Subserver <- Subservers],
  {noreply, State};
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

start_subserver(State) ->
  Count = list_to_binary(integer_to_list(State#state.subserversCount)),
  SubserverName = binary_to_atom(<<"broadcast_subserver_", Count/binary>>, utf8),
  {ok, _} = screen_broadcast_subserver_sup:start_subserver(SubserverName),
  Subservers = State#state.subservers ++ [SubserverName],
  {SubserverName, Subservers}.


get_current_subserver(State) ->
  case State#state.currentSubserver of
    undefined -> {State#state.subserversCount + 1, start_subserver(State)};
    _->
      case screen_broadcast_subserver:get_clients_count(State#state.currentSubserver) >= ?PROCESSES_TO_SUBSERVER of
        true -> {State#state.subserversCount + 1, start_subserver(State)};
        _ -> {State#state.subserversCount, {State#state.currentSubserver, State#state.subservers}}
      end
  end.


