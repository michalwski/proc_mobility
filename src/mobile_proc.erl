%% Author: michal.piotrowski
%% Created: 24-03-2012
%% Description: mobile_proc behavior
%%% @author Michal Piotrowski <michalwski@gmail.com>
%%% @copyright 2012 Michal Piotrowski
%%% @doc Behavior which mobile process has to implement.
%%% Below is list of required functions
%%% <ul>
%%% <li>init_state(term()) - is called after process migration</li>
%%% <li>send_me(node() | {tcp, Host:binary(), Port:integer()}) - sends process to destination point
%%% Should send message to process which is migrate
%%% and in message receive block logic of process migration should be implemented.
%%% see mobility_example:send_me and mobility_example:handle_call
%%% </li>
%%% <li>register() - Registers process in gproc module
%%% Should send message to mobile process as in send_me function
%%% see mobility_example
%%% </li>
%%% </ul>
%%% There is also optional function get_code() which returns code for mobile process
%%% This function is optional and will be called only if
%%% it is exported and there is no code to run process on destination node
%%% @end
-module(mobile_proc).


%%
%% Callbacks
%%

%% is called after process migration
-callback init_state(State::term()) ->
   ok.

%% Sends process to destination point
%% Should send message to process which is migrate
%% and in message receive block logic of process migration should be implemented
%% see mobility_example:send_me and mobility_example:handle_call
-callback send_me(Destination::node() | {tcp, Host::binary(), Port::integer()}) ->
    ok | {error, Reason::term()}.

%% Registeres process in gproc module
%% Should send message to mobile process as in send_me function
%% see mobility_example
-callback register() -> ok.

%% Returns code for mobile process
%% This function is optional and will be called only if
%% it is exported and there is no code to run process on destination node

-callback get_code() -> list().
