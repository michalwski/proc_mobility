%% Author: michal.piotrowski
%% Created: 24-03-2012
%% Description: TODO: Add description to mobile_proc
-module(mobile_proc).


%%
%% Callbacks
%%

%% @doc is called after process migration
-callback init_state(State::term()) ->
   ok.

%% @doc Sends process to destination point
%% Should send message to process which is migrate
%% and in message receive block logic of process migration should be implemented
%% @see mobility_example:send_me and mobility_example:handle_call
-callback send_me(Destination::node() | {tcp, Host::binary(), Port::integer()}) ->
    ok | {error, Reason::term()}.

%% @doc Registeres process in gproc module
%% Should send message to mobile process as in send_me function
%% @see mobility_example
-callback register() -> ok.

%% @doc Returns code for mobile process
%% This function is optional and will be called only if
%% it is implemented and there is no code to run process on destination node

-callback get_code() -> [binary()].
