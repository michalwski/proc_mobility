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
	gproc:send({p,g,Proc}, Msg).
whereis_name(Proc) ->
	case gproc:lookup_pids({p, g, Proc}) of
		[] -> unknown;
		List when is_list(List) ->
			lists:last(List);
		_ -> unknown
	end.
	
register_name(Name, Pid) when Pid == self() ->
	Result = gproc:reg({p,g,Name}),
	?INFO_MSG("registering name ~p for pid ~p -> ~p~n", [Name, Pid, Result]),
	Result.
unregister_name(Name) ->
	gproc:unreg({p, g, Name}).



%%
%% Local Functions
%%

