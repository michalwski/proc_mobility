%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michalwski@gmail.com>
%%% @copyright 2012 Michal Piotrowski
%%% @doc
%%% Transport layer interface. Defines following callbacks:
%%% <ul>
%%% <li>call(Target::term(), Message::term()) -> ok</li>
%%% <li>redirect_call(Name::atom(), Request::term(), From::term(), Target::term()) -> ok.</li>
%%% <li>redirect_cast(Name::atom(), Cast::term(), Target::term()) -> ok</li>
%%% <li>forward_messages(Msgs::term(), Name::atom(), Target::term()) -> ok</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(proc_mobility_transport).

%%
%% Callbacks
%%
-callback call(Target::tuple(), Message::term()) -> ok | {error, any()}.

-callback redirect_call(Name::atom(), Request::term(), From::tuple(), Target::tuple()) -> term().

-callback redirect_cast(Name::atom(), Cast::term(), Target::tuple()) -> no_return().

-callback forward_messages(Msgs::term(), Name::atom(), Target::tuple()) -> no_return().
