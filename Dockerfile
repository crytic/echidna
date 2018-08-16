FROM ubuntu:rolling
RUN apt-get update && apt-get -y upgrade
RUN apt-get install -y curl software-properties-common locales-all locales
RUN mkdir -p /etc/nix
RUN echo "build-users-group =" > /etc/nix/nix.conf
RUN curl https://nixos.org/nix/install | sh
RUN nix-env -i stack
COPY . /echidna/
WORKDIR /echidna
RUN stack setup && stack install
ENV PATH=$PATH:/root/.local/bin
RUN update-locale LANG=en_US.UTF-8
RUN locale-gen en_US.UTF-8  
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8
CMD ["echidna-test", "solidity/cli.sol"]
