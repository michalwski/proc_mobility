%%% -------------------------------------------------------------------
%%% Author  : michal
%%% Description :
%%%
%%% Created : 09-06-2012
%%% -------------------------------------------------------------------
-module(proc_proxy).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("proc_logging.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name, host, port}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Proc, Target) ->
	gen_server:start_link(?MODULE, [Proc, Target], []).

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
init([Proc, {Host, Port} = Target]) ->
	?INFO_MSG("Starting proxy for ~p located on ~p", [Proc, Target]),
	proc_mobility:register_name(Proc, self()),
    {ok, #state{name=Proc, host=Host, port=Port}}.

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
handle_call(Request, From, State) ->
	?INFO_MSG("Handle call ~p from ~p", [Request, From]),
    spawn(fun() -> redirect_call(Request, From, State) end),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	?INFO_MSG("Handle cast ~p", [Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	?INFO_MSG("Handle info ~p", [Info]),
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

redirect_call(Request, From, #state{name=Name, host=Host, port=Port}) ->
	{ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
	ok = gen_tcp:send(Socket, term_to_binary({proc_proxing, gen_call, Name, Request})),
	ok = inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, Bin} ->
			?INFO_MSG("Got ~p from socket ~p", [binary_to_term(Bin), Socket]),
			gen_server:reply(From, binary_to_term(Bin));
		{tcp_closed, Socket} ->
			?INFO_MSG("socket ~p closed by server", [Socket]);
		{Error} ->
			?ERROR_MSG("Got error ~p from socket ~p", [Error, Socket]),
			gen_server:reply(From, Error)
	end,
	ok = gen_tcp:close(Socket).