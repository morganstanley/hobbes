FROM redhat/ubi9:latest
ARG  DEPS
ENV  ARGS=-V
RUN  dnf update -y
RUN  dnf install -y ${DEPS}
CMD  ["sh", "-c", "mkdir -p /build && cd /build && cmake /src && VERBOSE=1 make -j2 && make test"]
