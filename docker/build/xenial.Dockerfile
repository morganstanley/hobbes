FROM ubuntu:xenial
ENV  ARGS -V
RUN  apt update
RUN  apt install -y cmake libedit-dev g++ llvm-3.7-dev libncurses5-dev zlib1g-dev libreadline-dev python2.7
CMD  mkdir -p /build && cd /build && cmake /src && make -j2 && make test
