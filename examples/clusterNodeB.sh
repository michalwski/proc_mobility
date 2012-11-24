#!/bin/bash
erl -sname b -pa ebin deps/*/ebin/ -config proc_mobility -m_tcp_port 1806 -eval \
	"application:start(sasl),net_adm:ping(erlang:list_to_atom(\"a@\"++net_adm:localhost())),application:start(gproc),application:start(proc_mobility)."
