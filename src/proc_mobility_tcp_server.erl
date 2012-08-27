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

-record(tcp_server_state, {clients, monitors}).
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
    {ok, {Port, ?TCP_OPTS}, #tcp_server_state{clients=dict:new(), monitors=dict:new()}}.

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
    %%?INFO_MSG("store home ~p for ~p", [{Host, Port}, PName]),
    {noreply, State#tcp_server_state{clients = dict:store(PName, {Host,Port}, State#tcp_server_state.clients)}};

handle_cast({proc_monitor, PName}, #tcp_server_state{monitors = Monitors} = State) ->
    State1 = case proc_mobility:whereis_name(PName) of
        unknown -> State;
        Pid ->
            Ref = monitor(process, Pid),
            State#tcp_server_state{monitors = dict:store(Ref, PName, Monitors)}
    end,
    {noreply, State1};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, normal}, #tcp_server_state{monitors = Monitors, clients = Clients} = State) -> %%there is possiblity that proccess was migrated
    PName = dict:fetch(Ref, Monitors),
    Addr = dict:fetch(PName, Clients),
    case proc_mobility:whereis_name(PName) of
        unknown -> 
            notify_process_terminated(PName, normal, Addr);
        _Pid -> 
            gen_server:cast(?PROCESSES_TCP_SERVER, {proc_monitor, PName})
    end,
    State#tcp_server_state{monitors = dict:erase(Ref, Monitors)};
handle_info({'DOWN', Ref, process, _Pid, Reason}, #tcp_server_state{monitors = Monitors, clients = Clients} = State) -> %%there is possiblity that proccess was migrated
    PName = dict:fetch(Ref, Monitors),
    Addr = dict:fetch(PName, Clients),
    notify_process_terminated(PName, Reason, Addr),
    State#tcp_server_state{monitors = dict:erase(Ref, Monitors)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Local Functions
%%

notify_process_terminated(PName, Reason, {Host, Port}) ->
    ok.

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
    pass_proc_deamon_call(Sock, Message),
    gen_server:cast(?PROCESSES_TCP_SERVER, {proc_monitor, PName});

handle_message(Sock, {proc_daemon, _ServerPort, Message}) ->
    pass_proc_deamon_call(Sock, Message);

handle_message(Sock, {proc_proxing, gen_call, Name, Request}) ->
    ServerReply = gen_server:call({via, proc_mobility, Name}, Request),
    gen_tcp:send(Sock, term_to_binary(ServerReply));

handle_message(_Sock, {proc_proxing, gen_cast, Name, Request}) ->
    gen_server:cast({via, proc_mobility, Name}, Request);

handle_message(Sock, {proc_proxing, is_alive, Name}) ->
    Response = case proc_mobility:whereis_name(Name) of
        unknown -> false;
        _ -> true
    end,
    gen_tcp:send(Sock, term_to_binary(Response));

handle_message(Sock, Message) ->
    ?INFO_MSG("Message ~p from Sock ~p", [Message, Sock]).

pass_proc_deamon_call(Sock, Message) ->
	?INFO_MSG("call to deamon ~p", [Message]),
	DaemonReply = gen_server:call(?PROCESSES_DAEMON, Message),
	gen_tcp:send(Sock, term_to_binary(DaemonReply)).

get_code(PName, Home, From) ->
    proc_mobility_utils:tcp_send_recv_reply(Home, {proc_daemon, proc_mobility:get_tcp_server_port(), {get_code, PName}}, From).
