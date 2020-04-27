if [ "$(uname)" != "Darwin" ]; then
    pip3 install crytic-compile --user
    pip3 install slither-analyzer --user
fi
