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
-export([start_link/0, 
         send/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("proc_mobility.hrl").
-record(pms_state, 
        {prepared=[], started=[], migrated=[]}).

%%%===================================================================
%%% API
%%%===================================================================
send(Pid, PState, Target) ->
    gen_server:call(?PROCESES_DAEMON, {send, Pid, PState, Target}).
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
handle_call({send, Pid, PState, Target}, From, State) ->
    ?INFO("send request pid ~p to ~p from ~p~n", [Pid, Target, From]),
    case gen_server:call({?PROCESES_DAEMON, Target}, {prepare_proc, Pid, PState}) of
        ok ->
            case gen_server:call({?PROCESES_DAEMON, Target}, {start_proc, Pid}) of
				ok -> {reply, ok, State#pms_state{migrated= State#pms_state.migrated ++ [Pid]}};
				_ -> 
					%%TODO log problem
					gen_server:call({?PROCESES_DAEMON, Target}, {clean_up, Pid}),
					{reply, false, State}
			end;
        _ ->
            %%TODO log problem
            {reply, false, State}
    end;
handle_call({prepare_proc, Pid, PState}, From, State) ->
    ?INFO("prepareing proc ~p", [Pid]),
    Reply = ok,
    {reply, Reply, State};
handle_call({start_proc, Pid}, From, State) ->
    ?INFO("starting proc ~p", [Pid]),
    Reply = ok,
    {reply, Reply, State};
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
