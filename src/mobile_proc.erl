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
-callback get_state() -> State::term().
-callback set_state(State::term()) ->
   ok.
%%
%% API Functions
%%

%%
%% Local Functions
%%

