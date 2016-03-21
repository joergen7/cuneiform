all:
	rebar3 escriptize

dev:
	rebar3 do escriptize, eunit, dialyzer, cover

clean:
	rebar3 clean

