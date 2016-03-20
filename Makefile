all:
	rebar3 do compile, eunit, dialyzer

clean:
	rebar3 clean

