FROM ubuntu:rolling
RUN    apt-get update \ 
    && apt-get -y upgrade \
    && apt-get install -y curl bzip2 gnupg perl bash sudo software-properties-common locales-all locales \
    && useradd -m -d /home/tester -s /bin/bash tester \
    && usermod -a -G sudo tester \
    && echo " tester      ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers \
    && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
    && locale-gen en_US.UTF-8

ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

WORKDIR /home/tester
COPY . /home/tester/echidna/
WORKDIR /home/tester/echidna
RUN chown -R tester .
USER tester
ENV USER tester

RUN curl https://nixos.org/nix/install | sh 

RUN    . /home/tester/.nix-profile/etc/profile.d/nix.sh \
    && nix-env -i stack \
    && stack setup \
    && stack install

ENV PATH=$PATH:/home/tester/.local/bin
CMD ["echidna-test", "solidity/cli.sol"]
