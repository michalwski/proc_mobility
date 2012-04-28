%% Author: michal.piotrowski
%% Created: 24-03-2012
%% Description: TODO: Add description to mobile_proc
-module(mobile_proc).

%%
%% Include files
%%

%%
%% Callbacks
%%
%% -callback get_state() -> State::term().
-callback init_state(State::term()) ->
   ok.
-callback send_me(Destination::term()) ->
	ok | {error, Reason::term()}.
%%
%% API Functions
%%

%%
%% Local Functions
%%

