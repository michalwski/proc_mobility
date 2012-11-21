%%
%%Mobile Proc State
%%
-record(mproc_state, {
	name::atom(),
	module::atom(), 
	state::term(), 
	code=[]::list()}).

-define(PROCESSES_DAEMON, proc_mobility_daemon).
-define(PROCESSES_TCP_CLIENT, proc_mobility_tcp_client).
-define(PROCESSES_TCP_SERVER, proc_mobility_tcp_server).
