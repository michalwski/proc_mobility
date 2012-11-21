%%% -------------------------------------------------------------------
%%% Author  : michal
%%% Description :
%%%
%%% Created : 09-06-2012
%%% -------------------------------------------------------------------
-module(proc_proxy_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_proxy/3, start_unnamed_proxy/3, start_link/0, stop_proxy/1]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc Starts processes proxies supervisor.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%% @doc Starts new named proxy for process migrated outside the cluster
-spec start_proxy(atom(), tuple(), module()) -> {ok, pid()}.
start_proxy(Proc, Target, Transport) ->
    supervisor:start_child(?SERVER, {Proc, {proc_proxy, start_link, [Proc, Target, Transport]}, transient, 100, worker, [proc_proxy]}).
%%% @doc Starts unnamed proxy for forwarding queued messages to migrated process.
-spec start_unnamed_proxy(atom(), tuple(), module()) -> {ok, pid()}.
start_unnamed_proxy(Proc, Target, Transport) ->
    supervisor:start_child(?SERVER, {Proc, {proc_proxy, start_unnamed_link, [Proc, Target, Transport]}, temporary, 100, worker, [proc_proxy]}).

%% @doc Stops given proxy.
-spec stop_proxy(pid()) -> no_return().
stop_proxy(Pid) ->
    gen_server:cast(Pid, proc_mobility_proxy_stop).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% @private
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok,{{one_for_one,5,10}, []}}.


