all: .rebar/cf_18.2.1_plt
	rebar co eu dialyze

clean:
	rebar clean

.rebar/cf_18.2.1_plt:
	rebar get-deps build-plt

