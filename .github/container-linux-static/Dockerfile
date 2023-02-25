FROM fpco/alpine-haskell-stack:9.2.5

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
    } >> /etc/stack/config.yaml
