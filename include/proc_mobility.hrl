%%
%%Mobile Proc State
%%
-record(mproc_state, {module, state, code=[]}).

-define(PROCESSES_DAEMON, proc_mobility_daemon).
-define(PROCESSES_PROXY, proc_mobility_proxy).

-define(INFO_MSG(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(INFO_MSG(Msg), error_logger:info_msg(Msg)).
-define(ERROR_MSG(Msg, Args), error_logger:error_msg(Msg, Args).
-define(ERROR_MSG(Msg), error_logger:error_msg(Msg)).
