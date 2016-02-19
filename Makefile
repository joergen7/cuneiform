all: .rebar/cf_18.2.1_plt
	rebar update-deps co eu dialyze doc

clean:
	rebar clean

.rebar/cf_18.2.1_plt:
	rebar update-deps build-plt

