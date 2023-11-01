FROM alpine:3.18 AS builder-echidna
ENV LD_LIBRARY_PATH=/usr/local/lib PREFIX=/usr/local HOST_OS=Linux
RUN echo "@testing https://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories && \
    apk add --no-cache \
        autoconf \
        automake \
        bash \
        build-base \
        bzip2-dev \
        cabal \
        cmake \
        curl \
        ghc \
        git \
        gmp-dev \
        libtool \
        ncurses-static \
        openssl-dev \
        readline-dev \
        stack@testing \
        sudo \
        zlib-dev \
        zlib-static

WORKDIR /echidna

COPY .github/scripts /echidna/.github/scripts/
RUN .github/scripts/install-libsecp256k1.sh && \
    .github/scripts/install-libff.sh

COPY . /echidna/
RUN mkdir -p /etc/stack &&\
    { cat /root/.stack/config.yaml ;\
      echo "system-ghc: true" ;\
      echo "install-ghc: false" ;\
      echo "skip-ghc-check: true" ;\
    } >> /etc/stack/config.yaml &&\
    chmod 664 /etc/stack/config.yaml
RUN stack install --flag echidna:static --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib


FROM ubuntu:focal AS builder-python3
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-suggests --no-install-recommends \
        gcc \
        python3.8-dev \
        python3.8-venv
ENV PIP_DISABLE_PIP_VERSION_CHECK=1
ENV PIP_NO_CACHE_DIR=1
RUN python3 -m venv /venv && /venv/bin/pip3 install --no-cache --upgrade setuptools pip
RUN /venv/bin/pip3 install --no-cache slither-analyzer solc-select


FROM gcr.io/distroless/python3-debian11:nonroot AS final-distroless
COPY --from=builder-echidna /root/.local/bin/echidna /usr/local/bin/echidna
COPY --from=builder-python3 /venv /venv
COPY docker/solc-install.py /usr/local/bin/solc-install
ENV PATH="$PATH:/venv/bin"
ENTRYPOINT [ "/usr/local/bin/solc-install", "/usr/local/bin/echidna" ]


FROM ubuntu:focal AS final-ubuntu
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-suggests --no-install-recommends \
        ca-certificates \
        curl \
        python3 \
        python3-distutils \
        && \
    rm -rf /var/lib/apt/lists/*
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
COPY --from=builder-echidna /root/.local/bin/echidna /usr/local/bin/echidna
RUN ln -s /usr/local/bin/echidna /usr/local/bin/echidna-test
COPY --from=builder-python3 /venv /venv
ENV LANG="C.UTF-8"
ENV PATH="$PATH:/venv/bin"
