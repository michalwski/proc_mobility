-module(proc_mobility_transport).

%%
%% Callbacks
%%
-callback call(Target::term(), Message::term()) -> ok.

-callback redirect_call(Name::atom(), Request::term(), From::term(), Target::term()) -> ok.

-callback redirect_cast(Name::atom(), Cast::term(), Target::term()) -> ok.

-callback forward_messages(Msgs::term(), Name::atom(), Target::term()) -> ok.
