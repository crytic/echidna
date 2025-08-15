FROM ubuntu:noble AS builder-echidna
ENV LD_LIBRARY_PATH=/usr/local/lib PREFIX=/usr/local HOST_OS=Linux
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-suggests --no-install-recommends \
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
RUN stack upgrade && stack setup && stack install --flag echidna:static --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib


FROM ubuntu:noble AS builder-python3
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-suggests --no-install-recommends \
        gcc \
        python3 \
        python3.12-dev \
        python3.12-venv
ENV PIP_DISABLE_PIP_VERSION_CHECK=1
ENV PIP_NO_CACHE_DIR=1
RUN python3 -m venv /venv && /venv/bin/pip3 install --no-cache --upgrade setuptools pip
RUN /venv/bin/pip3 install --no-cache slither-analyzer solc-select


FROM ubuntu:noble AS builder-solvers
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-suggests --no-install-recommends \
        unzip \
        ca-certificates \
        curl
RUN mkdir /solvers/
RUN if [ $(uname -m) = "aarch64" ]; then \
        curl -fsSL -o z3.zip https://github.com/Z3Prover/z3/releases/download/z3-4.15.2/z3-4.15.2-arm64-glibc-2.34.zip; \
    elif [ $(uname -m) = "x86_64" ]; then \
        curl -fsSL -o z3.zip https://github.com/Z3Prover/z3/releases/download/z3-4.15.2/z3-4.15.2-x64-glibc-2.39.zip; \
    fi && \
    unzip z3.zip && \
    cp -a z3-*/* /solvers/ && \
    rm -rf z3-*/ z3.zip
RUN if [ $(uname -m) = "aarch64" ]; then \
        curl -fsSL -o bitwuzla.zip https://github.com/bitwuzla/bitwuzla/releases/download/0.8.2/Bitwuzla-Linux-arm64-static.zip; \
    elif [ $(uname -m) = "x86_64" ]; then \
        curl -fsSL -o bitwuzla.zip https://github.com/bitwuzla/bitwuzla/releases/download/0.8.2/Bitwuzla-Linux-x86_64-static.zip; \
    fi && \
    unzip bitwuzla.zip && \
    cp -a Bitwuzla-*/* /solvers/ && \
    rm -rf Bitwuzla-*/ bitwuzla.zip


FROM gcr.io/distroless/python3-debian11:nonroot AS final-distroless
COPY --from=builder-echidna /root/.local/bin/echidna /usr/local/bin/echidna
COPY --from=builder-python3 /venv /venv
COPY docker/solc-install.py /usr/local/bin/solc-install
ENV PATH="$PATH:/venv/bin"
ENTRYPOINT [ "/usr/local/bin/solc-install", "/usr/local/bin/echidna" ]


FROM ubuntu:noble AS final-ubuntu
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-suggests --no-install-recommends \
        ca-certificates \
        curl \
        python3 \
        && \
    rm -rf /var/lib/apt/lists/*
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
COPY --from=builder-solvers /solvers/bin/z3 /solvers/bin/bitwuzla /usr/local/bin/
COPY --from=builder-echidna /root/.local/bin/echidna /usr/local/bin/echidna
RUN ln -s /usr/local/bin/echidna /usr/local/bin/echidna-test
COPY --from=builder-python3 /venv /venv
ENV LANG="C.UTF-8"
ENV PATH="$PATH:/venv/bin"
