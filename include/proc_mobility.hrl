%%
%%Mobile Proc State
%%
-record(mproc_state, {module, state}).

-define(PROCESES_DAEMON, proc_mobility_daemon).
-define(INFO(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(INFO(Msg), error_logger:info_msg(Msg)).
