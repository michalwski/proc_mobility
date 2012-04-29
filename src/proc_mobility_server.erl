%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michalwski@gmail.com>
%%% @copyright 2012 Michal Piotrowski
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(proc_mobility_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("proc_mobility.hrl").
-record(pms_state, 
        {prepared=[], starting=[]}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?PROCESES_DAEMON}, ?MODULE, [], []).

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
    {ok, #pms_state{}}.

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
handle_call({send, Proc, PState, Target}, From, State) when is_record(PState, mproc_state) ->
    ?INFO_MSG("send request Proc ~p to ~p with ~p from ~p~n", [Proc, Target, PState, From]),
    case gen_server:call({?PROCESES_DAEMON, Target}, {prepare_proc, Proc, PState}) of
        ok ->
            case gen_server:call({?PROCESES_DAEMON, Target}, {start_proc, Proc}) of
				ok -> {reply, ok, State};
				Result -> 
					?INFO_MSG("cannot start prepared process ~p", [Result]),
					gen_server:call({?PROCESES_DAEMON, Target}, {clean_up, Proc}),
					{reply, {error, cannot_start}, State}
			end;
        Result ->
            ?INFO_MSG("Cannot prepare process ~p", [Result]),
            {reply, {error, cannot_prepare}, State}
    end;

handle_call({send, _Proc, PState, _Target}, _From, State) ->
	?INFO_MSG("incorrect PState type, should be record mproc_state, get ~p", [PState]),
	{reply, {error, state_type_error}, State};

handle_call({prepare_proc, Proc, PState}, From, State) ->
    ?INFO_MSG("Prepareing proc ~p from ~p", [Proc, From]),
	case proplists:get_value(Proc, State#pms_state.prepared) of
		undefined ->
			M = PState#mproc_state.module,
			S = PState#mproc_state.state,
			?INFO_MSG("Preparing proces in module ~p with state ~p", [M, S]),
			Listener = apply(M, init_state, [S]),
			?INFO_MSG("Proces initiated, listener set ~p", [Listener]),
		    {reply, ok, State#pms_state{prepared= State#pms_state.prepared ++ [{Proc, {Listener, M}}]}};
		_ ->
			{reply, {error, already_prepared}, State}
	end;

handle_call({start_proc, Proc}, From, State) ->
    ?INFO_MSG("Starting proc ~p", [Proc]),
	case proplists:get_value(Proc, State#pms_state.prepared) of
		undefined ->
			{reply, {error, unprepared}, State};
		{Listener, Module} ->
			?INFO_MSG("Running prepared proc ~p ~p", [Listener, From]),
			run_prepared(Listener),
			{noreply, State#pms_state{starting = State#pms_state.starting ++ [{Listener, {From, Proc, Module}}], prepared = proplists:delete(Proc, State#pms_state.prepared)}}
	end;

handle_call({started, Listener}, From, State) ->
	?INFO_MSG("listener ~p stared new process from ~p", [Listener, From]),
	case proplists:get_value(Listener, State#pms_state.starting) of
		undefined -> 
			{reply, {errpr, notstarting}, State};
		{Caller, Proc, Module} ->
			?INFO_MSG("proc ~p started, give response to caller ~p", [Proc, Caller]),
			gen_server:reply(Caller, ok),
			gproc:unregister_name(Proc),
			apply(Module, register,[]),
			{reply, ok, State#pms_state{starting = proplists:delete(Listener, State#pms_state.starting)}}
	end;

handle_call(Request, From, State) ->
    io:format("unrecognized request ~p from ~p~n", [Request, From]),
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

run_prepared(Listener) ->
	Listener ! {mobility, run}.