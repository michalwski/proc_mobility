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
-include("proc_logging.hrl").
%%
%% Exported Functions
%%
-export([send/2, started/1, migrate/2, register_name/2, unregister_name/1, whereis_name/1, get_tcp_server_port/0]).

-type destination() :: node() | {tcp, binary(), integer()}.
%%
%% API Functions
%%

%% @doc sends process to given destination
%% Destination can be a node in erlang cluster
%% or node outside the cluster but with proc_mobility_tcp_server started
-spec migrate(term(), destination()) -> ok | {error, term()}.
migrate(PState, Target) when is_atom(Target) ->
    gen_server:call(?PROCESSES_DAEMON, {send, PState, {?PROCESSES_DAEMON, Target}, proc_mobility_server});
migrate(PState, {tcp, Host, Port}) ->
	gen_server:call(?PROCESSES_DAEMON, {send, PState, {Host, Port}, proc_mobility_tcp_client}).
			
started(Listener) ->
	gen_server:call(?PROCESSES_DAEMON, {started, Listener}).
send(Proc, Msg) ->
	gproc:send({p,g,Proc}, Msg).

%% @doc Return pid of mobile process	
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

get_tcp_server_port() ->
	case init:get_argument(m_tcp_port) of
		{ok, [[Port0]]} -> list_to_integer(Port0);
		_ -> 1805
	end.


