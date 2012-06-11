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
-callback init_state(State::term()) ->
   ok.

-callback send_me(Destination::term()) ->
	ok | {error, Reason::term()}.

-callback register() -> ok.

-callback get_code() -> [binary()].
%%
%% API Functions
%%

%%
%% Local Functions
%%

