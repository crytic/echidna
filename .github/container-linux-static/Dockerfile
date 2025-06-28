FROM docker.io/benz0li/ghc-musl:9.8.4
# https://gitlab.com/benz0li/ghc-musl

RUN apk upgrade --no-cache &&\
    apk add --no-cache \
        autoconf \
        automake \
        binutils-gold \
        cmake \
        libtool

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
    { echo "system-ghc: true" ;\
      echo "install-ghc: false" ;\
      echo "skip-ghc-check: true" ;\
    } >> /etc/stack/config.yaml &&\
    chmod 664 /etc/stack/config.yaml &&\
    chown root:runneruser /etc/stack/config.yaml
