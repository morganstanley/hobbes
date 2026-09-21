FROM ubuntu:noble
ARG  DEPS
ENV  ARGS -V
RUN  apt update
RUN  apt install -y ${DEPS}
CMD  mkdir -p /build && cd /build && cmake /src && VERBOSE=1 make -j2 && make test
