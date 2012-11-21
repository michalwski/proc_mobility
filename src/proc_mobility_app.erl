%% Author: Michal Piotrowski <michalwski@gmail.com>
%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michalwski@gmail.com>
%%% @copyright 2012 Michal Piotrowski
%%% @doc
%%% Proc_mobility application.
%%% Starts required process and supervisors responsible for processes migration.
%%% @end
%%%-------------------------------------------------------------------


-module(proc_mobility_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc Starts proc_mobility application
start(_StartType, _StartArgs) ->
    proc_mobility_sup:start_link().

%% @doc Stops proc_mobility application
stop(_State) ->
    ok.
