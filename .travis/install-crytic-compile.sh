if [ "$(uname)" != "Darwin" ]; then

    rm -Rf crytic-compile
    git clone https://github.com/crytic/crytic-compile --depth 1
    cd crytic-compile
    python setup.py install
    cd ..

fi
