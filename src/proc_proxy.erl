%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michalwski@gmail.com>
%%% @copyright 2012 Michal Piotrowski
%%% @doc
%%% Mobile process proxy.
%%% Used in forwarding messages to mobile process migrated outside cluster.
%%% Is also used to forward messages queued during migration process.
%%% @end
%%%-------------------------------------------------------------------
-module(proc_proxy).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("proc_logging.hrl").
-include("proc_mobility.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/3, start_unnamed_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name, target, transport}).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc Starts named process proxy for processes migrated outside cluster.
-spec start_link(atom(), tuple(), module()) -> {ok, pid()}.
start_link(Proc, Target, TransportLayer) ->
    gen_server:start_link(?MODULE, [Proc, Target, true, TransportLayer], []).

%% @doc Starts unnamed process proxy used only for forwarding queued messages
-spec start_unnamed_link(atom(), tuple(), module()) -> {ok, pid()}.
start_unnamed_link(Proc, Target, TransportLayer) ->
    gen_server:start_link(?MODULE, [Proc, Target, false, TransportLayer], []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @private
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([PName, Target, Named, TransportLayer]) ->
    ?INFO_MSG("Starting proxy for ~p located on ~p", [PName, Target]),
    case Named of
        false -> ok;
        true -> proc_mobility:register_name(PName, self())
    end,
    {ok, #state{name=PName, target = Target, transport=TransportLayer}}.

%% --------------------------------------------------------------------
%% @private
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, #state{name=Name, target=Target, transport=Transport} = State) ->
    spawn(fun() -> Transport:redirect_call(Name, Request, From, Target) end),
    {noreply, State}.

%% --------------------------------------------------------------------
%% @private
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(proc_mobility_proxy_stop, State) ->
    {stop, normal, State};
handle_cast(Msg, #state{name=Name, target=Target, transport=Transport} = State) ->
    spawn(fun() -> Transport:redirect_cast(Name, Msg, Target) end),
    {noreply, State}.

%% --------------------------------------------------------------------
%% @private
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, #state{name=Name, target=Target, transport=Transport} = State) ->
    spawn(fun() -> Transport:redirect_cast(Name, Info, Target) end),
    {noreply, State}.

%% --------------------------------------------------------------------
%% @private
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% @private
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

