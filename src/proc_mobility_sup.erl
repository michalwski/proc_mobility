
-module(proc_mobility_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Port = proc_mobility:get_tcp_server_port(),
	io:format("Port to use ~p~n", [Port]),
	{ok, { {one_for_one, 5, 10}, [
								  ?CHILD(proc_mobility_server, proc_mobility_server, worker, []),
								  ?CHILD(proc_mobility_tcp_server, proc_mobility_tcp_server, worker, [Port]),
								  ?CHILD(proc_mobility_tcp_client, proc_mobility_tcp_client, worker, [])
								  
								  ]}}.
%%     {ok, { {one_for_one, 5, 10}, []} }.

