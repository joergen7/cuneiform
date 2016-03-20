all:
	rebar3 do escriptize, eunit, dialyzer

clean:
	rebar3 clean

