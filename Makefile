all:
	erlc -I include -o ebin -Wall -d src/*.erl
	cp proc_mobility.app ebin
