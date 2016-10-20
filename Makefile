PWD=$(shell pwd)

all: compile

install: compile
	rm -f /usr/local/bin/cuneiform
	echo rlwrap $(PWD)/_build/default/bin/cuneiform \$$@ > /usr/local/bin/cuneiform
	chmod a+x /usr/local/bin/cuneiform

compile:
	rebar3 escriptize

dev:
	rebar3 do escriptize, eunit, dialyzer, cover, edoc

clean:
	rm -rf .rebar
	rm -rf _build
	find doc -not -name 'overview.edoc' -print0 | xargs -0 rm --
	#rm -rf doc
	rm -f rebar.lock
