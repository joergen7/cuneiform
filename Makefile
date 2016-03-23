all:
	rebar3 do compile, escriptize

dev:
	rebar3 do compile, escriptize, eunit, dialyzer, cover, edoc

clean:
	rm -rf .rebar
	rm -rf _build
	rm -rf doc
	rm -f rebar.lock

