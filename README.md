proc_mobility
=============
Proc_mobility is an application adding mobility mechanism to erlang processes.
Using this application process can be migrate inside and outside Erlang cluster.
Migration outside cluster is done over plain TCP connection to other Erlang node on which proc mobility application is also launched.

Requirements
-------------------
Application requirements
To launch proc_mobility applications gproc application is needed. It is used to registering mobile processes.

Mobile process
Mobile processes should implement "mobile_proc" behavior (@see mobile_proc), sample implementation can be found in src/mobility_example.erl. @see mobility_example 
It is very important to implement logic of "send_me" and "register" functions in receive message block to be sure that it is done by mobile process itself and not the caller of that function.