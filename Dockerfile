# docker image build -t cfl:latest .
# docker container run -it cfl:latest

FROM alpine:3.17.2
LABEL maintainer="Jörgen Brandt <joergen@cuneiform-lang.org>"

ENV BIN_PATH /usr/local/bin

WORKDIR /tmp
RUN apk add --update git erlang erlang-dev erlang-dialyzer octave racket bash elixir gnuplot openjdk17 python3 R perl nodejs
RUN git clone https://github.com/erlang/rebar3.git && (cd rebar3 && ./bootstrap) && cp rebar3/rebar3 $BIN_PATH && rm -rf rebar3
RUN git clone https://github.com/joergen7/cuneiform.git && (cd cuneiform && rebar3 escriptize) && cp cuneiform/_build/default/bin/cfl $BIN_PATH && rm -rf cuneiform
RUN git clone https://github.com/joergen7/cre.git && (cd cre && rebar3 escriptize) # && cp cre/_build/default/bin/cre $BIN_PATH && rm -rf cre
RUN git clone https://github.com/joergen7/cf_client.git && (cd cf_client && rebar3 escriptize) && cp cf_client/_build/default/bin/cfl_client $BIN_PATH && rm -rf cf_client
RUN git clone https://github.com/joergen7/cf_worker.git && (cd cf_worker && rebar3 escriptize) && cp cf_worker/_build/default/bin/cfl_wrk $BIN_PATH && rm -rf cf_worker
RUN git clone https://github.com/joergen7/effi.git && (cd effi && rebar3 escriptize) && cp effi/_build/default/bin/effi $BIN_PATH && rm -rf effi

ENTRYPOINT ["sh"]