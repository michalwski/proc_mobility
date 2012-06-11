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
-include("proc_logging.hrl").
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
handle_call({send, Proc, PState, Target, TransportLayer}, From, State) when is_record(PState, mproc_state) ->
	spawn(fun() -> gen_server:reply(From, move_proc(TransportLayer, Proc, PState, Target)) end),
    {noreply, State};

handle_call({send, _Proc, PState, _Target}, _From, State) ->
	?INFO_MSG("incorrect PState type, should be record mproc_state, get ~p", [PState]),
	{reply, {error, state_type_error}, State};

handle_call({move_proc, Proc, PState}, From, State) ->
    ?INFO_MSG("Prepareing proc ~p from ~p", [Proc, From]),
	spawn(fun() -> gen_server:reply(From, prepare_and_run(PState)) end),
    {noreply, State};


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

move_proc(TransportLayer, Proc, PState, Target) ->
	?INFO_MSG("send request Proc ~p to ~p ~n", [Proc, Target]),
    case TransportLayer:call(Target, {move_proc, Proc, PState}) of
        ok ->
			ok;
		Result -> 
			?INFO_MSG("cannot start process ~p on ~p", [Result, Target]),
			{error, cannot_start}
    end.

prepare_and_run(PState) ->
	M = PState#mproc_state.module,
	S = PState#mproc_state.state,
	Each = fun({Module, Binary, Filename}) ->
			Loaded = code:load_binary(Module, Filename, Binary),
			?INFO_MSG("code loading ~p~n", [Loaded])
		end,
	
	lists:foreach(Each, PState#mproc_state.code),
	
	?INFO_MSG("Preparing proces in module ~p with state ~p", [M, S]),
	case apply(M, init_state, [S]) of
		ok ->
			apply(M, register, []),
			ok;
		Error ->
			?ERROR_MSG("Cannot initiate process ~p", [Error]),
			Error
	end.
