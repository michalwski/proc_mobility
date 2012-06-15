%% Author: michal
%% Created: 20-05-2012
%% Description: TODO: Add description to proc_mobility_tcp_server
-module(proc_mobility_tcp_server).

-behaviour(gen_listener_tcp).

-define(TCP_OPTS, [binary, inet,
                   {active,    false},
                   {backlog,   10},
                   {nodelay,   true},
                   {packet,    4},
                   {reuseaddr, true}]).

%%
%% Include files
%%

-include("proc_mobility.hrl").
-include("proc_logging.hrl").

-record(tcp_server_state, {clients}).
%%
%% Exported Functions
%%

-export([start_link/1]).

-export([init/1,
         handle_accept/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%
%% API Functions
%%

start_link(Port) ->
	gen_listener_tcp:start_link({local, ?PROCESSES_TCP_SERVER}, ?MODULE, [Port], []).

init([Port]) ->
	{ok, {Port, ?TCP_OPTS}, #tcp_server_state{clients=dict:new()}}.

handle_accept(Sock, State) ->
	Pid = spawn(fun() -> handle_message(Sock) end),
	gen_tcp:controlling_process(Sock, Pid),
	{noreply, State}.

handle_call({get_code, PName}, From, State) ->
	case dict:find(PName, State#tcp_server_state.clients) of
		{ok, Home} ->
			?INFO_MSG("need to contact with ~p to get ~p code", [Home, PName]),
			spawn(fun() -> get_code(PName, Home, From) end),
			{noreply, State};
		_ ->
			?ERROR_MSG("unknown destination for ~p", [PName]),
			{reply, {error, unknown_home}}
	end;

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast({proc_home, PName, {{ok,{Host,_}},Port}}, State) ->
	?INFO_MSG("store home ~p for ~p", [{Host, Port}, PName]),
	{noreply, State#tcp_server_state{clients = dict:store(PName, {Host,Port}, State#tcp_server_state.clients)}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Local Functions
%%

handle_message(Sock) ->
	ok = inet:setopts(Sock, [{active, once}]),
	receive
		{tcp, Socket, Data} ->
			handle_message(Socket, binary_to_term(Data)),
			handle_message(Sock);
		{tcp_closed, _Socket} ->
			?INFO_MSG("Client Disconected")
	end.

handle_message(Sock, {proc_daemon, ServerPort, {move_proc, #mproc_state{name=PName}} = Message}) ->
	gen_server:cast(?PROCESSES_TCP_SERVER, {proc_home, PName, {inet:peername(Sock), ServerPort}}),
	pass_proc_deamon_call(Sock, Message);

handle_message(Sock, {proc_daemon, _ServerPort, Message}) ->
	pass_proc_deamon_call(Sock, Message);

handle_message(Sock, {proc_proxing, gen_call, Name, Request}) ->
	ServerReply = gen_server:call(Name, Request),
	gen_tcp:send(Sock, term_to_binary(ServerReply));

handle_message(_Sock, {proc_proxing, gen_cast, Name, Request}) ->
	gen_server:cast(Name, Request);

handle_message(Sock, Message) ->
	?INFO_MSG("Message ~p from Sock ~p", [Message, Sock]).

pass_proc_deamon_call(Sock, Message) ->
	?INFO_MSG("call to deamon ~p", [Message]),
	DaemonReply = gen_server:call(?PROCESSES_DAEMON, Message),
	gen_tcp:send(Sock, term_to_binary(DaemonReply)).

get_code(PName, Home, From) ->
	proc_mobility_utils:tcp_send_recv_reply(Home, {proc_daemon, proc_mobility:get_tcp_server_port(), {get_code, PName}}, From).