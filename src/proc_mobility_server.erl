%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michalwski@gmail.com>
%%% @copyright 2012 Michal Piotrowski
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(proc_mobility_server).

-behaviour(gen_server).
-behaviour(proc_mobility_transport).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% for transport layer
-export([forward_messages/3, call/2, redirect_call/4, redirect_cast/3]).

-include("proc_mobility.hrl").
-include("proc_logging.hrl").
-record(pms_state, 
        {moving, starting}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?PROCESSES_DAEMON}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #pms_state{moving=dict:new(), starting=dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send, #mproc_state{name=Proc} = PState, Target, TransportLayer}, From, State) when is_record(PState, mproc_state) ->
    case dict:is_key(Proc, State#pms_state.moving) of
        true ->
            {reply, already_moving, State};
        _ ->
            spawn(fun() -> 
                        Reply = case move_proc(TransportLayer, PState, Target) of
                            ok ->
                                {Pid, _ } = From,
                                TransportLayer:forward_messages(erlang:process_info(Pid, messages), Proc, Target),
                                ok;
                            R -> R
                        end,
                        gen_server:reply(From, Reply) 
                end),
            {noreply, State#pms_state{moving = dict:store(Proc, PState, State#pms_state.moving)}}
    end;

handle_call({send, PState, _Target}, _From, State) ->
	?INFO_MSG("incorrect PState type, should be record mproc_state, get ~p", [PState]),
	{reply, {error, state_type_error}, State};

handle_call({move_proc, #mproc_state{name=Proc} = PState}, From, State) ->
    ?INFO_MSG("Prepareing proc ~p from ~p", [Proc, From]),
	spawn(fun() -> gen_server:reply(From, prepare_and_run(From, PState)) end),
    {noreply, State};

handle_call({get_code, PName}, From, State) ->
	case dict:find(PName, State#pms_state.moving) of
		error ->
			?ERROR_MSG("Request for code for proc ~p which is not moving", [PName]),
			{reply, {error, no_such_starting_process}, State};
		{ok, #mproc_state{module=Module}} ->
			?INFO_MSG("Request for code for module ~p from ~p", [Module, From]),
			spawn(fun() -> gen_server:reply(From, get_code(Module)) end),
			{noreply, State}
	end;

handle_call(Request, From, State) ->
    ?INFO_MSG("unrecognized request ~p from ~p~n", [Request, From]),
    Reply = false,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({sent, PName}, State) ->
	case dict:is_key(PName, State#pms_state.moving) of
		false ->
			{noreply, State};
		_ ->
			{noreply, State#pms_state{moving = dict:erase(PName, State#pms_state.moving)}}
	end;

handle_cast({not_sent, PName}, State) ->
	case dict:is_key(PName, State#pms_state.moving) of
		false ->
			{noreply, State};
		_ ->
			{noreply, State#pms_state{moving = dict:erase(PName, State#pms_state.moving)}}
	end;

handle_cast({started, PName}, State) ->
	case dict:is_key(PName, State#pms_state.starting) of
		false ->
			{noreply, State};
		_ ->
			{noreply, State#pms_state{starting = dict:erase(PName, State#pms_state.starting)}}
	end;

handle_cast({not_started, PName}, State) ->
	case dict:is_key(PName, State#pms_state.starting) of
		false ->
			{noreply, State};
		_ ->
			{noreply, State#pms_state{starting = dict:erase(PName, State#pms_state.starting)}}
	end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================================
%%% Transport Layer API
%%%==================================================================
call(Target, Message) ->
    gen_server:call(Target, Message).

forward_messages(undefinded, _, _ ) -> ok;
forward_messages({messages, []}, _, _) -> ok;
forward_messages({messages, Msgs}, PName, {_, Target}) ->
    {ok, Pid} = proc_proxy_sup:start_unnamed_proxy(PName, Target, ?MODULE),
    ?INFO_MSG("forwarding messages ~p", [Msgs]),
    forward_messages1(Msgs, Pid).

redirect_call(Name, Request, {Pid, _} = From, Target) ->
    Reply = gen_server:call({Name, Target}, Request),
    ?INFO_MSG("reply ~p to ~p which is ~p", [Reply, From, erlang:process_info(Pid, status)]),
    gen_server:reply(From, Reply).

redirect_cast(Name, Cast, Target) ->
    ?INFO_MSG("redirect cast to ~p on ~p", [Name, Target]),
    {Name, Target} ! Cast.

%%%===================================================================
%%% Internal functions
%%%===================================================================

forward_messages1([], Pid) -> 
    timer:sleep(50), %%to ensure that all eventual reply msgs will be send before stop msg
    gen_server:cast(Pid, prox_mobility_proxy_stop),
    ok;
forward_messages1([Msg | Msgs], Proc) ->
    Proc ! Msg,
    forward_messages1(Msgs, Proc).


move_proc(TransportLayer, #mproc_state{name=Proc} = PState, Target) ->
	?INFO_MSG("send request Proc ~p to ~p ~n", [Proc, Target]),
    case TransportLayer:call(Target, {move_proc, PState}) of
        ok ->
			gen_server:cast(?PROCESSES_DAEMON, {started, Proc}),
			ok;
		Error -> 
			?INFO_MSG("cannot start process ~p on ~p", [Error, Target]),
			gen_server:cast(?PROCESSES_DAEMON, {not_started, Proc}),
			{error, cannot_start}
    end.

prepare_and_run({Pid, _Tag}, PState) ->
	M = PState#mproc_state.module,
	S = PState#mproc_state.state,
	Each = fun({Module, Binary, Filename}) ->
			Loaded = code:load_binary(Module, Filename, Binary),
			?INFO_MSG("code loading ~p~n", [Loaded])
		end,
	
	lists:foreach(Each, PState#mproc_state.code),
	
	?INFO_MSG("Preparing proces in module ~p with state ~p", [M, S]),
	try
		run_proc(M, PState)
	catch
		throw:Term ->
			?ERROR_MSG("Cannot run new proc becouse of throw ~p", [Term]),
			{throw, Term};
		exit:Reason ->
			?ERROR_MSG("Cannot run new proc becouse of exit ~p", [Reason]),
			{exit, Reason};
		error:undef ->
			LocalNode = node(),
			case code:is_loaded(M) of
				false ->
					Addr = case node(Pid) of
						LocalNode -> %%Pid is from local node and module is not loaded so it comes from tcp transport
							?PROCESSES_TCP_SERVER;	
						Node -> {?PROCESSES_DAEMON, Node}
					end,
					?INFO_MSG("Need code for module ~p from ~p", [M, Pid]),
					Code = gen_server:call(Addr, {get_code, PState#mproc_state.name}),
					lists:foreach(Each, Code),
					try
						run_proc(M, PState)
					catch
						Class:Reason ->
							?ERROR_MSG("Cannot run proc after code loading ~p:~p~n~p", [Class, Reason, erlang:get_stacktrace()]),
							{Class, Reason}
					end;
				_ ->
					{error, undef}
			end;
		error:Reason ->
			?ERROR_MSG("Cannot run new proc ~p ~n~p", [Reason, erlang:get_stacktrace()]),
			{error, Reason}
	end.

run_proc(Module, PState) ->
	case apply(Module, init_state, [PState#mproc_state.state]) of
		ok ->
			%%apply(Module, register, []),
			gen_server:cast(?PROCESSES_DAEMON, {started, PState#mproc_state.name}),
			ok;
		Error ->
			?ERROR_MSG("Cannot initiate process ~p", [Error]),
			gen_server:cast(?PROCESSES_DAEMON, {not_started, PState#mproc_state.name}),
			Error
	end.

get_code(Module) ->
	try
		Module:get_code()
	catch
		error:undef ->
			?WARN_MSG("module ~p does not define function get_code, try to get code for the module", [Module]),
			[code:get_object_code(Module)]
	end.
