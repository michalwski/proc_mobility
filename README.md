proc_mobility
=============
Proc_mobility is an application adding mobility mechanism to erlang processes.
Using this application process can be migrate inside and outside Erlang cluster.
Migration outside cluster is done over plain TCP connection to other Erlang node on which proc mobility application is also launched.
## Functionalities ##
- transparent migration, process communicating with mobile process dose not know where exactly process resides at the moment.
- migration outside and Erlang cluster, mobile process can be easily migrated to other Erlang cluster if only destination cluster supports process migrations.
 - for now only gen_server is fully supported but gen_fsm and gen_event can be easily added
- easy in use

## Requirements ##

### Application requirements ###
To launch proc_mobility applications gproc application is needed. It is used to registering mobile processes.

### Mobile process ###
Mobile processes should implement "mobile_proc" behavior, sample and simple implementation can be found in `src/mobility_example.erl`.
It is very important to implement logic of `send_me` and `register` functions in receive message block to be sure that it is done by mobile process itself and not the caller of that function.

## Quick start ##
### Inside cluster ###
In examples folder there are scripts running two Erlang nodes and connecting them in a cluster.
- clusterNodeA.sh - run as first - lunches node `a` and starts proc_mobility application and mobility_example,
- clusterNodeB.sh - run as second - lunches node `b`, connects to node `a` and starts proc_mobility application 

After running both script, tape following command in the first one:

	mobility_example:send_me(hd(nodes())).

It will migrate mobility_example process to node `b`.

### Outside cluster ###
In examples folder there are also scripts running Erlang nodes and starting migration TCP server:
 - tcpNodeA.sh - starts Erlang node and mobility TCP server on port 1805
 - tcpNodeB.sh - starts Erlang node and mobility TCP server on port 1806

After running both script (in any order) tape following command in the first node:

	mobility_example:send_me({tcp, "localhost", 1806}).

It will connect to mobility server listening on port 1806 and send mobility_example process to that node.

## Documentation ##
To generate code documentation just run:

	make docs
