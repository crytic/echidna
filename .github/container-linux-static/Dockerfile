FROM alpine:3.18.6
# Based on https://github.com/fpco/alpine-haskell-stack/blob/9.2.8v2/ghc-Dockerfile

RUN apk upgrade --no-cache &&\
    apk add --no-cache \
        curl \
        gcc \
        git \
        libc-dev \
        xz \
        gmp-dev \
        autoconf \
        automake \
        binutils \
        build-base \
        coreutils \
        cpio \
        linux-headers \
        libffi-dev \
        musl-dev \
        zlib-dev \
        zlib-static \
        ncurses-dev \
        ncurses-libs \
        ncurses-static \
        bash \
        lld \
        shadow # for stack --docker, provides groupadd

RUN curl -sSLo /usr/local/bin/stack https://github.com/commercialhaskell/stack/releases/download/v2.13.1/stack-2.13.1-linux-x86_64-bin && \
    chmod +x /usr/local/bin/stack

# https://stackoverflow.com/a/41517423
RUN ln -s /usr/lib/libncurses.a /usr/lib/libtinfo.a

COPY stack-config.yaml /root/.stack/config.yaml

RUN cd /tmp && \
    curl -sSLo /tmp/ghc.tar.xz https://downloads.haskell.org/~ghc/9.6.5/ghc-9.6.5-x86_64-alpine3_12-linux.tar.xz && \
    tar xf ghc.tar.xz && \
    cd ghc-9.6.5-x86_64-unknown-linux && \
    ./configure --prefix=/usr/local && \
    make install && \
    rm -rf /tmp/ghc.tar.xz /tmp/ghc-9.6.5-x86_64-unknown-linux

RUN apk upgrade --no-cache &&\
    apk add --no-cache \
        cmake \
        libtool \
        openssl-dev \
        tar \
        zlib-dev \
        zlib-static

RUN addgroup \
        --gid 115 \
        runneruser \
    && \
    adduser \
        --disabled-password \
        --gecos "" \
        --ingroup runneruser \
        --home /github/home \
        --uid 1001 \
        runneruser

RUN mkdir -p /etc/stack &&\
    { cat /root/.stack/config.yaml ;\
      echo "system-ghc: true" ;\
      echo "install-ghc: false" ;\
      echo "skip-ghc-check: true" ;\
    } >> /etc/stack/config.yaml &&\
    chmod 664 /etc/stack/config.yaml &&\
    chown root:runneruser /etc/stack/config.yaml
