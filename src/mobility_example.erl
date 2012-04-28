%%% -------------------------------------------------------------------
%%% Author  : michal
%%% Description :
%%%
%%% Created : 26-04-2012
%%% -------------------------------------------------------------------
-module(mobility_example).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("proc_mobility.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([]).

%% mobile_proc callbacks
-export([init_state/1, send_me/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

-export([start/0]).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_with_state(State) ->
	gen_server:start({local, ?MODULE}, ?MODULE, {mobility, State}, []).

%% ====================================================================
%% Mobile Proc functions
%% ====================================================================

init_state(State) ->
	RunListener = fun() ->
		?INFO_MSG("Running listener ~p", self()),
		receive
			{mobility, run, Caller, Pid} ->
				Status = start_with_state(State),
				?INFO_MSG("started with state ~p and got ~p", [State, Status]),
				proc_mobility_server:started(Pid, Caller)
		end,
		?INFO_MSG("Listern finished")
	end,
	spawn(RunListener).

send_me(Destination) ->
	gen_server:call(?MODULE, {mobility, send_me, Destination}).

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
	?INFO_MSG("Starting"),
    {ok, {ala, ma, kota}};

init({mobility, State}) ->
	{ok, State}.
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

handle_call({mobility, send_me, Destination}, From, State) ->
	case proc_mobility_server:send(?MODULE, #mproc_state{module=?MODULE, state=State}, Destination) of
		ok ->
			{stop, migrated, ok, State};
		Result -> 
			{reply, Result, State}
	end;
	
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
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
	?INFO_MSG("proces terminated with reason ~p and state ~p", [Reason, State]),
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

