if [ "$(uname)" != "Darwin" ]; then
    sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
    sudo apt-get update
    sudo apt-get install gcc-7 g++-7
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-7 10
    sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-7 10
fi
