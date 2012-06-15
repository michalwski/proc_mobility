%%% -------------------------------------------------------------------
%%% Author  : michal
%%% Description :
%%%
%%% Created : 31-05-2012
%%% -------------------------------------------------------------------
-module(proc_mobility_tcp_client).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("proc_mobility.hrl").
-include("proc_logging.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, call/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(pp_state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?PROCESSES_TCP_CLIENT}, ?MODULE, [], []).

call(Target, {move_proc, Proc} = Message) ->
	?INFO_MSG("Message ~p", [Message]),
	case gen_call(Target, Message) of
		ok ->
			%%need to register new process
			gen_server:cast(?PROCESSES_TCP_CLIENT, {register, Proc, Target}),
			ok;
		Any -> Any
	end;
call(Target, Message) ->
	gen_call(Target, Message).

gen_call(Target, Message) ->
	gen_server:call(?PROCESSES_TCP_CLIENT, {Target, {proc_daemon, proc_mobility:get_tcp_server_port(), Message}}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	proc_proxy_sup:start_link(),
	{ok, #pp_state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({ {_,_} = Target, Message}, From, State) ->
	spawn(fun() ->
				  proc_mobility_utils:tcp_send_recv_reply(Target, Message, From)
		  end),
	
	{noreply, State};


handle_call(Request, _From, State) ->
	?ERROR_MSG("Uknown request ~p", [Request]),
	Reply = unknown,
	{reply, Reply, State}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({register, Proc, Target}, State) ->
	proc_proxy_sup:start_proxy(Proc, Target),
	{noreply, State};
handle_cast(Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

