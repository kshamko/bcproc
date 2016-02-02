%%%-------------------------------------------------------------------
%%% Description module bcproc
%%%-------------------------------------------------------------------
%%% Module provides interface access to the whole library.
%%% It encapsulates all the functions required to perform,
%%% manage and manipulate a broadcast. Please don't use functions from
%%% other library modules in your applications.
%%%-------------------------------------------------------------------
%%% Basic usage
%%% -------------------------------------------------------------------
%%% 0. Start bcproc application
%%% 1. Start a so called brodcaster with bcproc:start_bc/1. This will
%%% start an empty pool of broadcast servers.
%%% 2. Add clients to to the broadcaster with bcproc:add_client/2. This
%%% will add a client's process PID to a broadcast server from the pool
%%% 3. Make a broadcast with bcproc:broadcast/2. After that all the client
%%% processes added to the broadcaster will recieve a message {text, SOME_MESSAGE}
%%%-------------------------------------------------------------------
%%% LIST OF TO DOes
%%%-------------------------------------------------------------------
%%% - bcproc_bpool:get_current_bcserver/1
%%% - bcproc_broadcast, ?CLEANUP_TIME
%%% - improve cleaner (or do smth else) to change current broadcast server to a server with less clients
%%%-------------------------------------------------------------------
-module(bcproc).

-author("Konstantin Shamko").
-author_email("konstantin.shamko@gmail.com").

%% API
-export([
  start_bc/1,
  stop_bc/1,
  add_client/2,
  remove_client/2,
  broadcast/2
]).

%%----------------------------------------------------------------------
%% Function: start_bc/1
%% Purpose: Starts a broadcaster - an empty pool of broadcast servers.
%% Args:   Name - atom with a broadcaster name.Name is used to generate broadcaster
%%         process name to register it.
%% Returns: A tuple {ok, Pid} or {error, Reason}
%%----------------------------------------------------------------------
start_bc(Name) when is_atom(Name) ->
  bcproc_sup:start_broadcaster(Name);
start_bc(_Name) ->
  {error, bad_name}.

%%----------------------------------------------------------------------
%% Function: stop_bc/1
%% Purpose: Stops a broadcaster and kill all broadcast servers in it's pool
%% Args:   Name - atom with a broadcaster name.
%% Returns: An atom 'ok' or tuple {error, Reason}
%%----------------------------------------------------------------------
stop_bc(Name) when is_atom(Name) ->
  bcproc_sup:stop_broadcaster(Name);
stop_bc(_Name) ->
  {error, bad_name}.

%%----------------------------------------------------------------------
%% Function: add_client/2
%% Purpose: Adds client (Pid or Name) to a broadcaster with BcName. Please note
%%          that there is no check if broadcaster (or client) exists.
%% Args:   BcName - atom with a broadcaster name.
%%         Client - pid or name of the process to sent broadcast message to
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%%----------------------------------------------------------------------
add_client(BcName, Client) ->
  bcproc_bpool:add_client(BcName, Client).

%%----------------------------------------------------------------------
%% Function: remove_client/2
%% Purpose: Removes client (Pid or Name) from a broadcaster with BcName. Please note
%%          that there is no check if broadcaster (or client) exists.
%% Args:   BcName - atom with a broadcaster name.
%%         Client - pid or name of the process to sent broadcast message to and which was
%%                  added to a broadcaster before
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%%----------------------------------------------------------------------
remove_client(BcName, Client) ->
  bcproc_bpool:remove_client(BcName, Client).

%%----------------------------------------------------------------------
%% Function: broadcast/2
%% Purpose: Tells broadcaster to broadcast message Msg to all clients added to the broadcaster.
%%          Please note  that there is no check if broadcaster exists. All client processes
%%          will recieve {bcast, Msg} message. I.e. gen_server can handle it wih handle_info/3
%%          callback
%% Args:   BcName - atom with a broadcaster name.
%%         Client - pid or name of the process to sent broadcast message to
%% Returns: An atom 'ok' - basically this is a return of gen_server:cast
%%----------------------------------------------------------------------
broadcast(BcName, Msg) ->
  bcproc_bpool:broadcast(BcName, Msg).

