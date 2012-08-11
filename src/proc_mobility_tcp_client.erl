%%% -------------------------------------------------------------------
%%% Author  : michal
%%% Description :
%%%
%%% Created : 31-05-2012
%%% -------------------------------------------------------------------
-module(proc_mobility_tcp_client).

-behaviour(gen_server).
-behaviour(proc_mobility_transport).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("proc_mobility.hrl").
-include("proc_logging.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% transport layer callbacks
-export([call/2, forward_messages/3, redirect_call/4, redirect_cast/3]).

-record(pp_state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?PROCESSES_TCP_CLIENT}, ?MODULE, [], []).


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
handle_call({register, Proc, Target}, _From, State) ->
    proc_proxy_sup:start_proxy(Proc, Target, ?MODULE),
    {reply, ok, State};

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
handle_cast(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%% Transport Layer Functions
%% --------------------------------------------------------------------
call(Target, Message) ->
    gen_call(Target, Message).

forward_messages(Msgs, PName, Target) ->
    gen_server:call(?PROCESSES_TCP_CLIENT, {register, PName, Target}),
    forward_messages0(Msgs, PName).

redirect_call(Name, Request, From, {Host, Port}) ->
    proc_mobility_utils:tcp_send_recv_reply({Host, Port},
                                            {proc_proxing, gen_call, Name, Request},
                                            From).

redirect_cast(Name, Msg, {Host, Port}) ->
    proc_mobility_utils:tcp_send({Host, Port}, {proc_proxing, gen_cast, Name, Msg}).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

gen_call(Target, Message) ->
    gen_server:call(?PROCESSES_TCP_CLIENT, {Target, {proc_daemon, proc_mobility:get_tcp_server_port(), Message}}).

forward_messages0(undefined, _) -> ok;
forward_messages0({messages, []}, _) -> ok;
forward_messages0({messages, Msgs}, PName) ->
    forward_messages0(Msgs, PName);
forward_messages0([], _) -> 
    timer:sleep(50),
    ok;
forward_messages0([Msg | Msgs], Proc) ->
    proc_mobility:send(Proc, Msg),
    forward_messages0(Msgs, Proc).
