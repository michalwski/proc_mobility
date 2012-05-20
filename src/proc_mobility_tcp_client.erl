%% Author: michal
%% Created: 20-05-2012
%% Description: TODO: Add description to proc_mobility_tcp_client
-module(proc_mobility_tcp_client).

%%
%% Include files
%%
-include("proc_mobility.hrl").
%%
%% Exported Functions
%%
-export([call/2]).

%%
%% API Functions
%%
call({Host, Port}, Message) ->
	?INFO_MSG("Sending").


%%
%% Local Functions
%%

