%% Author: michal
%% Created: 24-03-2012
%% Description: TODO: Add description to mobility_example
-module(mobility_example).
-behaviur(mobile_proc).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([set_state/1, get_state/0, start/0, loop/1]).

%%
%% API Functions
%%
start()->
	spawn(?MODULE, loop, [1]).

get_state() ->
	23.

set_state(State) ->
	loop(State),
	ok.

%%
%% Local Functions
%%
loop(State) ->
	io:format("~p~n", [State]),
	timer:sleep(30000),
	loop(State+1).
