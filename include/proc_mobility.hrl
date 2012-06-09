%%
%%Mobile Proc State
%%
-record(mproc_state, {module, state, code=[]}).

-define(PROCESSES_DAEMON, proc_mobility_daemon).
-define(PROCESSES_TCP_CLIENT, proc_mobility_tcp_client).
