FROM ubuntu:rolling
RUN apt-get update && apt-get -y upgrade
RUN apt-get install -y curl libgmp-dev libbz2-dev libreadline-dev software-properties-common
RUN add-apt-repository -y ppa:ethereum/ethereum
RUN apt-get update
RUN apt-get install -y solc
RUN curl -sSL https://get.haskellstack.org/ | sh
COPY . /echidna/
WORKDIR /echidna
RUN stack upgrade && stack setup && stack install
ENV PATH=$PATH:$HOME/.local/bin
CMD ["echidna-test", "solidity/cli.sol"]
