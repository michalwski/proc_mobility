%%
%%Mobile Proc State
%%
-record(mproc_state, {module, state, code=[]}).

-define(PROCESES_DAEMON, proc_mobility_daemon).
-define(INFO_MSG(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(INFO_MSG(Msg), error_logger:info_msg(Msg)).
-define(ERROR_MSG(Msg), error_logger:error_msg(Msg)).
-define(ERROR_MSG(Msg, Args), error_logger:error_msg(Msg, Args).
