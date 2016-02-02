%%%-------------------------------------------------------------------
%%% @author konstantin.shamko
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2016 3:23 PM
%%%-------------------------------------------------------------------
-module(bcproc_tests).
-author("konstantin.shamko").

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

pool_start_test_() ->
  {"Broadcast pool can be started and registered",
    {setup,
      fun start_bc/0,
      fun stop_bc/1,
      fun process_exists/1
    }
  }.

pool_stop_test_() ->
  {"Broadcast pool can be stopped and unregistered",
    {setup,
      fun start_stop_bc/0,
      fun stop_bc/1,
      fun process_not_exists/1
    }
  }.

pool_add_clients_test_() ->
  {"Clients can be added to pool",
    {setup,
      fun start_bc_with_clients/0,
      fun stop_bc/1,
      fun clients_added/1
    }
  }.

pool_remove_clients_test_() ->
  {"Clients can be removed from pool",
    {setup,
      fun start_bc_with_clients/0,
      fun stop_bc/1,
      fun clients_removed/1
    }
  }.

broadcast_test_() ->
  {"Test broadcast",
    {setup,
      fun start_bc_with_clients/0,
      fun stop_bc/1,
      fun broadcast/1
    }
  }.

cleaner_test_() ->
  {"Test cleaner of dead processes",
    {setup,
      fun start_bc_with_clients/0,
      fun stop_bc/1,
      fun clean/1
    }
  }.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start_bc() ->
  application:start(bcproc),
  Name = test_broadcast,
  {ok, Pid} = bcproc:start_bc(Name),
  {Name, Pid}.

start_stop_bc() ->
  {Name, Pid} = start_bc(),
  bcproc:stop_bc(Name),
  {Name, Pid}.

start_bc_with_clients() ->
  {Name, Pid} = start_bc(),
  Clients =  add_clients(Name, 400, []),
  {Name, Pid, Clients}.

stop_bc({BcName, _Pid}) ->
  bcproc:stop_bc(BcName),
  application:stop(bcproc);
stop_bc({BcName, Pid, _Clients}) ->
  stop_bc({BcName, Pid}).


add_clients(_BcName, 0, Clients) ->
  timer:sleep(1 * 1000),
  Clients;
add_clients(BcName, Count, Clients) ->
  Pid = spawn(dummy_client()),
  bcproc:add_client(BcName, Pid),
  add_clients(BcName, Count - 1, [Pid] ++ Clients).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%%============================================
%%
%%============================================
process_exists({BcName, Pid}) ->
  [
    ?_assert(erlang:is_process_alive(Pid)),
    ?_assertEqual(Pid, whereis(BcName))
  ].

%%============================================
%%
%%============================================
process_not_exists({BcName, Pid}) ->
  [
    ?_assertEqual(false, erlang:is_process_alive(Pid)),
    ?_assertEqual(undefined, whereis(BcName))
  ].

%%============================================
%%
%%============================================
clients_added({BcName, _Pid, Clients}) ->
  SupName = bcproc_broadcast_sup:get_sup_name(BcName),
  LatestPool = test_broadcast_sup_latest_child,
  CountClients1 = bcproc_broadcast:get_clients_count(LatestPool),

  [ClientToAdd] = lists:sublist(Clients, 1, 1),
  bcproc:add_client(BcName, ClientToAdd),
  timer:sleep(500),

  CountClients2 = bcproc_broadcast:get_clients_count(LatestPool),

  [_,_,_,{workers, CountWorkers}] = supervisor:count_children(SupName),

  [
    ?_assertEqual(150, CountClients1),
    ?_assertEqual(150, CountClients2),
    ?_assertEqual(test_broadcast_sup, SupName),
    ?_assertEqual(2, CountWorkers)
  ].

%%============================================
%%
%%============================================
clients_removed({BcName, _Pid, Clients}) ->
  ClientsToRemove = lists:sublist(Clients, 1, 99),
  remove_client(ClientsToRemove, BcName),

  LatestPool = test_broadcast_sup_latest_child,
  [
    ?_assertEqual(51, bcproc_broadcast:get_clients_count(LatestPool))
  ].

%%============================================
%%
%%============================================
broadcast({BcName, _Pid, Clients}) ->
  ets:new(bcast, [named_table, public]),
  ets:insert(bcast, {count, 0}),

  bcproc:broadcast(BcName, test_message),
  timer:sleep(1 * 1000),

  [{count, Count}] = ets:lookup(bcast, count),

  [
    ?_assertEqual(400, Count)
  ].

%%============================================
%%
%%============================================
clean({_BcName, _Pid, Clients}) ->

  [ClientToKill] = lists:sublist(Clients, 19, 1),
  exit(ClientToKill, kill),
  LatestPool = test_broadcast_sup_latest_child,

  {ok, CleanerPid} = bcproc_cleaner_sup:start_cleaner(),
  {state, _A, ClientsDict, _BcName} = sys:get_state(LatestPool),

  bcproc_cleaner_server:clean(CleanerPid, ClientsDict, LatestPool),
  timer:sleep(500),
  CountClients1 = bcproc_broadcast:get_clients_count(LatestPool),

  [ClientToKill1] = lists:sublist(Clients, 99, 1),
  exit(ClientToKill1, kill),
  LatestPool ! cleanup,
  timer:sleep(500),
  CountClients2 = bcproc_broadcast:get_clients_count(LatestPool),

  bcproc_broadcast:set_clean_pids(LatestPool, dict:new(), 0),
  timer:sleep(100),
  CountClients3 = bcproc_broadcast:get_clients_count(LatestPool),

  [
    ?_assertEqual(149, CountClients1),
    ?_assertEqual(148, CountClients2),
    ?_assertEqual(0, CountClients3)
  ].


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
remove_client([Client|Clients], BcName) ->
  bcproc:remove_client(BcName, Client),
  remove_client(Clients, BcName);
remove_client([], _BcName) ->
  timer:sleep(1*1000),
  done.

dummy_client() ->
  fun() ->
    receive
      {bcast, test_message} ->
        ets:update_counter(bcast, count, {2, 1}),
        ok;
      Msg -> Msg
    end
  end.