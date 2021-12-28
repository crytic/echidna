FROM debian:bullseye AS builder-echidna
ENV LD_LIBRARY_PATH=/usr/local/lib PREFIX=/usr/local HOST_OS=Linux
RUN apt-get update && \
    apt-get install -y --no-install-suggests --no-install-recommends \
        cmake \
        curl \
        git \
        libbz2-dev \
        libgmp-dev \
        libreadline-dev \
        libsecp256k1-dev \
        libssl-dev \
        software-properties-common \
        sudo
RUN curl -sSL https://get.haskellstack.org/ | sh
COPY . /echidna/
WORKDIR /echidna
RUN .github/scripts/install-libff.sh
RUN stack upgrade && stack setup && stack install --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib

FROM debian:bullseye AS builder-python3
RUN apt-get update && \
    apt-get install -y --no-install-suggests --no-install-recommends \
        gcc \
        python3.9-dev \
        python3.9-venv
ENV PIP_DISABLE_PIP_VERSION_CHECK=1
ENV PIP_NO_CACHE_DIR=1
RUN python3 -m venv /venv && /venv/bin/pip3 install --no-cache slither-analyzer solc-select

FROM gcr.io/distroless/python3-debian11:nonroot AS final
COPY --from=builder-echidna /root/.local/bin/echidna-test /usr/local/bin/echidna-test
COPY --from=builder-python3 /venv /venv
COPY docker/solc-install.py /usr/local/bin/solc-install
ENV PATH="$PATH:/venv/bin"
ENTRYPOINT [ "/usr/local/bin/solc-install", "/usr/local/bin/echidna-test" ]
