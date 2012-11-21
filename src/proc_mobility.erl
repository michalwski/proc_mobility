%% Author: Michal Piotrowski <michalwski@gmail.com>
%% Created: 29-04-2012
%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michalwski@gmail.com>
%%% @copyright 2012 Michal Piotrowski
%%% @doc
%%% Main module exporting API to process migration.
%%% To use this mechanism functions from this module should be used.
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

-type mproc_state() :: #mproc_state{}.

-type destination() :: node() | {tcp, binary(), integer()}.

-export_type([destination/0]).
%%
%% API Functions
%%

%% @doc sends process to given destination
%% Destination can be a node in erlang cluster
%% or node outside the cluster but with proc_mobility_tcp_server started
-spec migrate(mproc_state(), destination()) -> ok | {error, term()}.
migrate(PState, Target) when is_atom(Target) ->
    gen_server:call(?PROCESSES_DAEMON, {send, PState, {?PROCESSES_DAEMON, Target}, proc_mobility_server});
migrate(PState, {tcp, Host, Port}) ->
	gen_server:call(?PROCESSES_DAEMON, {send, PState, {Host, Port}, proc_mobility_tcp_client}).
			
started(Listener) ->
	gen_server:call(?PROCESSES_DAEMON, {started, Listener}).

%% Functions needed for registering and calling gen_servers via this module

%% @doc Sends message to mobile process
%% Calling process location is transparent, it can be on the same node, other node in cluster 
%% or outside the cluster if mobility mechanism is configured to work over plain TCP connections
-spec send(atom(), any()) -> any().
send(Proc, Msg) ->
	gproc:send({p,g,Proc}, Msg).

%% @doc Return pid of mobile process
%% It can be distributed process pid or process proxy pid if migrated outside Erlang cluster	
-spec whereis_name(atom()) -> pid() | unknown.
whereis_name(Proc) ->
	case gproc:lookup_pids({p, g, Proc}) of
		[] -> unknown;
		List when is_list(List) ->
			lists:last(List)
	end.

%% @doc Registers given mobile process.
%% Has to be called by registering process 	
-spec register_name(atom(), pid()) -> boolean().
register_name(Name, Pid) when Pid == self() ->
	Result = gproc:reg({p,g,Name}),
	?INFO_MSG("registering name ~p for pid ~p -> ~p~n", [Name, Pid, Result]),
	Result.
-spec unregister_name(atom()) -> boolean().
%% @doc Unregister previously registered mobile process name
unregister_name(Name) ->
	gproc:unreg({p, g, Name}).

%% @doc Returns current node TCP port number on which listens for migration requests outside cluster
-spec get_tcp_server_port() -> integer().
get_tcp_server_port() ->
	case init:get_argument(m_tcp_port) of
		{ok, [[Port0]]} -> list_to_integer(Port0);
		_ -> 1805
	end.


