PWD=$(shell pwd)

all: compile

install: compile
	ln -sf $(PWD)/_build/default/bin/cuneiform /usr/local/bin/cuneiform

compile:
	rebar3 escriptize

dev:
	rebar3 do escriptize, eunit, dialyzer, cover, edoc

clean:
	rm -rf .rebar
	rm -rf _build
	rm -rf doc
	rm -f rebar.lock

