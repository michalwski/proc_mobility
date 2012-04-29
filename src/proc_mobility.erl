%% Author: Michal Piotrowski <michalwski@gmail.com>
%% Created: 29-04-2012
%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michalwski@gmail.com>
%%% @copyright 2012 Michal Piotrowski
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(proc_mobility).

%%
%% Include files
%%
-include("proc_mobility.hrl").
%%
%% Exported Functions
%%
-export([send/2, started/1, migrate/3, register_name/2, unregister_name/1, whereis_name/1]).

%%
%% API Functions
%%
migrate(Proc, PState, Target) ->
    gen_server:call(?PROCESES_DAEMON, {send, Proc, PState, Target}).
started(Listener) ->
	gen_server:call(?PROCESES_DAEMON, {started, Listener}).
send(Proc, Msg) ->
	gproc:send({n,g,Proc}, Msg).
whereis_name(Proc) ->
	gproc:whereis_name({n, g, Proc}).
register_name(Name, Pid) when Pid == self() ->
	?INFO_MSG("registering name ~p for pid ~p", [Name, Pid]),
	gproc:add_global_name(Name).
unregister_name(Name) ->
	gproc:unregister_name({n, g, Name}).



%%
%% Local Functions
%%

