%%%-------------------------------------------------------------------
%%% Description module bcproc_app
%%%-------------------------------------------------------------------
%%% Implementation of application behaviour for bcproc
%%%-------------------------------------------------------------------
-module(bcproc_app).

-author("Konstantin Shamko").
-author_email("konstantin.shamko@gmail.com").

-behaviour(application).

%% Application callbacks
-export([
  start/0,
  start/2,
  stop/1
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  start(normal, []).

start(_StartType, _StartArgs) ->
  bcproc_sup:start_link().

stop(_State) ->
  ok.
