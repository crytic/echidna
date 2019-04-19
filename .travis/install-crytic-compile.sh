if [ "$(uname)" != "Darwin" ]; then

    git clone https://github.com/crytic/crytic-compile --depth 1
    cd crytic-compile
    python setup.py install
    cd ..

fi
