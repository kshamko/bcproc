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

-export([add_clients/2]).
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
  io:format("ZZZZ~n"),
  add_clients(Name, 260),
  {Name, Pid}.

stop_bc({BcName, _Pid}) ->
  bcproc:stop_bc(BcName),
  application:stop(bcproc).



add_clients(_BcName, 0) ->
  done;
add_clients(BcName, Count) ->
  Pid = spawn(dummy_client()),
  io:format("Pid ~p~n", [Pid]),

  bcproc:add_client(BcName, Pid),
  add_clients(BcName, Count - 1).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

process_exists({BcName, Pid}) ->
  [
    ?_assert(erlang:is_process_alive(Pid)),
    ?_assertEqual(Pid, whereis(BcName))
  ].

process_not_exists({BcName, Pid}) ->
  [
    ?_assertEqual(false, erlang:is_process_alive(Pid)),
    ?_assertEqual(undefined, whereis(BcName))
  ].

clients_added({BcName, Pid}) ->

  SupName = bcproc_broadcast_sup:get_sup_name(BcName),
  [_,_,_,{workers, CountWorkers}] = supervisor:count_children(SupName),
  [
    ?_assertEqual(test_broadcast_sup, SupName),
    ?_assertEqual(2, CountWorkers)
  ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
dummy_client() ->
  fun() ->
    receive
      Msg -> Msg
    end
  end.