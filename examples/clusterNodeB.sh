#!/bin/bash
erl -sname b -pa ebin deps/*/ebin/ -config proc_mobility -m_tcp_port 1806 -eval \
	"application:start(sasl),net_adm:ping('a@michal'),application:start(gproc),application:start(proc_mobility)."
