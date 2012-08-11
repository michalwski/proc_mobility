#!/bin/bash
erl -sname a -pa ebin deps/*/ebin/ -config proc_mobility -boot start_sasl \
	-eval "application:start(gproc), application:start(proc_mobility), mobility_example:start()."
