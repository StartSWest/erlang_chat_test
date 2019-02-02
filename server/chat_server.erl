%%%-------------------------------------------------------------------
%% @author Ivan Carmenates Garcia
%% @copyright (C) 2019
%% @doc
%% Chat Server for Erlang Test.
%% @end
%%%-------------------------------------------------------------------
-module(chat_server).
-behaviour(application).

%% API
-export([
  start/0
]).

%% Application callbacks
-export([
  start/2,
  stop/1]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
  observer:start(),
  io:format("~n~n> Distributed server node started.~n"),
  ok = application:start(chat_server).

%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
  chat_server_sup:start_link().

stop(_State) ->
  ok.