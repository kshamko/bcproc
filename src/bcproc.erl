%%%-------------------------------------------------------------------
%%% @author konstantin.shamko
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2015 11:51 AM
%%%-------------------------------------------------------------------
-module(bcproc).
-author("konstantin.shamko").

%% API
-export([start_server/1, stop_server/1, add_client/2, remove_client/2, broadcast/2]).


start_server(ServerName) ->
  bcproc_sup:start_server(ServerName).


stop_server(ServerName) ->
  bcproc_sup:stop_server(ServerName).


add_client(ServerName, ClientPid) when is_pid(ClientPid) ->
  bcproc_broadcast_server:add_client(ServerName, ClientPid).


remove_client(ServerName, ClientPid) when is_pid(ClientPid) ->
  bcproc_broadcast_server:remove_client(ServerName, ClientPid).


broadcast(ServerName, Msg) ->
  bcproc_broadcast_server:broadcast(ServerName, Msg).

