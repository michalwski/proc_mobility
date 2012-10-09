proc_mobility
=============
proc_mobility is an application adding mobility mechanism to erlang processes.

Requirements
-------------------
Mobile processes should implement "mobile_proc" behavior, sample implementation can be found in src/mobility_example.erl. 
It is very important to implement logic of "send_me" and "register" functions in receive message block to be sure that it is done by mobile process and not the caller of that function.
Quickstart guide
---------------- 
