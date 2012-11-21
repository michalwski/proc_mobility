DIALYZER = dialyzer

.PHONY: deps

all: deps compile

compile: rebar
	./rebar compile
deps: rebar
	./rebar get-deps
	
clean: rebar
	./rebar clean

docs: rebar clean-docs
	./rebar doc skip_deps=true

clean-docs:
	rm -rf doc/*.html
	rm -rf doc/*.png
	rm -rf doc/*.css
	rm -rf doc/edoc-info
	
rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl inets crypto public_key ssl deps/*

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs