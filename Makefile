.PHONY: deps

all: deps compile

compile: rebar
	./rebar compile
deps: rebar
	./rebar get-deps
	
clean: rebar
	./rebar clean
	
rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar