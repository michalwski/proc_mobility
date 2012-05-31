%% Author: michal
%% Created: 20-05-2012
%% Description: TODO: Add description to proc_mobility_tcp_server
-module(proc_mobility_tcp_server).

-behaviour(gen_listener_tcp).

-define(TCP_OPTS, [binary, inet,
                   {active,    false},
                   {backlog,   10},
                   {nodelay,   true},
                   {packet,    raw},
                   {reuseaddr, true}]).

%%
%% Include files
%%

-include("proc_mobility.hrl").

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
	gen_listener_tcp:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
	{ok, {Port, ?TCP_OPTS}, nil}.

handle_accept(Sock, State) ->
	Pid = spawn(fun() -> handle_message(Sock) end),
	gen_tcp:controlling_process(Sock, Pid),
	{noreply, State}.

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

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
			handle_message(Socket, binary_to_term(Data));
		{tcp_closed, _Socket} ->
			?INFO_MSG("Client Disconected")
	end.

handle_message(Sock, {proc_daemon, Message}) ->
	DaemonReply = gen_server:call(?PROCESSES_DAEMON, Message),
	gen_tcp:send(Sock, term_to_binary(DaemonReply));
handle_message(Sock, Message) ->
	?INFO_MSG("Message ~p from Sock ~p", [Message, Sock]).