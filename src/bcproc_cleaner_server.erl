%%%-------------------------------------------------------------------
%%% Description module bcproc_broadcast
%%%-------------------------------------------------------------------
%%% Actually this module processes contain clients pids to broadcast to
%%%-------------------------------------------------------------------
-module(bcproc_cleaner_server).

-author("Konstantin Shamko").
-author_email("konstantin.shamko@gmail.com").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  clean/3
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%----------------------------------------------------------------------
%% Function: clean/3
%% Purpose: Removes dead pids fron ProcDict
%% Args:   Pid - pid of cleaner process
%%         ProcDict - dictionary of pids to clean
%%         BcServer - broadcast server to send clran pids to
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%%----------------------------------------------------------------------
clean(Pid, ProcDict, BcServer) ->
  gen_server:cast(Pid, {clean, ProcDict, BcServer}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Handling cast messages
%%--------------------------------------------------------------------
handle_cast({clean, ProcDict, SubserverPid}, State) ->
  CleanProc = clean_proc_list(dict:to_list(ProcDict), ProcDict),
  bcproc_broadcast:set_clean_pids(SubserverPid, CleanProc, dict:size(CleanProc)),
  {stop, normal, State};
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
terminate(normal, _State)->
  normal;
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
%% Function: clean_proc_list/2
%% Purpose: Removes dead pids from list of pids
%% Args:   Pids - list of pid.
%%         Dict - dictionary of pids
%% Returns: Dict - dictionary of pids
%%----------------------------------------------------------------------
clean_proc_list([{Pid,_}|Pids], Dict) ->
  case process_info(Pid) of
    undefined -> clean_proc_list(Pids, dict:erase(Pid, Dict));
    _ -> clean_proc_list(Pids, Dict)
  end;
clean_proc_list([], Dict) ->
  Dict.