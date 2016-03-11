all:
	rebar3 compile
	rebar3 eunit
	rebar3 dialyzer

clean:
	rebar3 clean

