FROM ubuntu:rolling
RUN apt-get update && apt-get -y upgrade
RUN apt-get install -y curl bzip2 gnupg perl bash sudo software-properties-common locales-all locales

RUN useradd -m -d /home/tester -s /bin/bash tester
RUN usermod -a -G sudo tester
RUN echo " tester      ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers

RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

WORKDIR /home/tester
USER tester
ENV USER tester

RUN curl https://nixos.org/nix/install | sh

COPY . /home/tester/echidna/
WORKDIR /home/tester/echidna

RUN . /home/tester/.nix-profile/etc/profile.d/nix.sh && \
    nix-env -i stack && stack setup && stack install

ENV PATH=$PATH:/home/tester/.local/bin
CMD ["echidna-test", "solidity/cli.sol"]
