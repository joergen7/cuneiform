XRL=cf_scan cf_prescan
YRL=cf_parse
SRC=cf_cre cf_sem cf_shell cf_sup condor cuneiform cuneiform_app gen_queue lib_os local
INC=cuneiform
PWD=$(shell pwd)

all: _build/default/bin/cuneiform

install: _build/default/bin/cuneiform
	ln -sf $(PWD)/_build/default/bin/cuneiform /usr/local/bin/cuneiform

_build/default/bin/cuneiform: $(SRC:%=src/%.erl) $(INC:%=include/%.hrl) $(XRL:%=src/%.xrl) $(YRL:%=src/%.yrl)
	rebar3 escriptize

dev:
	rebar3 do escriptize, eunit, dialyzer, cover, edoc

clean:
	rm -rf .rebar
	rm -rf _build
	rm -rf doc
	rm -f rebar.lock

