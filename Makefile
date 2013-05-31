build: deps
	./rebar compile

deps:
	./rebar init-deps
	./rebar get-deps

run: build
	./run.sh

clean:
	./rebar clean
