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
-export([start_server/1, stop_server/1]).


start_server(Name) ->
  bcproc_sup:start_server(Name).


stop_server(Name) ->
  bcproc_sup:stop_server(Name).


broadcast(ServerName, Msg) ->
  ok.

