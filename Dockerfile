FROM ubuntu:bionic AS builder
RUN apt-get update && apt-get -y upgrade && apt-get install -y sudo cmake curl libgmp-dev libssl-dev libbz2-dev libreadline-dev software-properties-common libsecp256k1-dev
RUN apt-get update
RUN curl -sSL https://get.haskellstack.org/ | sh
COPY . /echidna/
WORKDIR /echidna
ENV LD_LIBRARY_PATH=/usr/local/lib TRAVIS_OS_NAME=linux
RUN .travis/install-libff.sh
RUN stack upgrade && stack setup && stack install --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib

FROM ubuntu:bionic AS final
WORKDIR /root
COPY --from=builder /root/.local/bin/echidna-test /root/.local/bin/echidna-test
COPY .travis/install-crytic-compile.sh .travis/install-crytic-compile.sh
RUN apt-get update && apt-get -y upgrade && apt-get install -y wget git locales-all locales python3.6 python3-setuptools
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.6 10
RUN wget https://github.com/ethereum/solidity/releases/download/v0.4.25/solc-static-linux && chmod +x solc-static-linux && mv solc-static-linux /usr/bin/solc-0.4.25
RUN wget https://github.com/ethereum/solidity/releases/download/v0.5.7/solc-static-linux && chmod +x solc-static-linux && mv solc-static-linux /usr/bin/solc
ENV TRAVIS_OS_NAME=linux
RUN .travis/install-crytic-compile.sh
RUN update-locale LANG=en_US.UTF-8 && locale-gen en_US.UTF-8
ENV PATH=$PATH:/root/.local/bin LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8
CMD ["/bin/bash"]
