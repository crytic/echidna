#! /bin/bash

set -eu

export PYTHONPATH=/root/.local/lib/python3.6/site-packages

OPTIONS="contract config format corpus-dir test-limit shrink-limit seq-len \
contract-addr deployer sender seed crytic-args solc-args"

SWITCHES="check-asserts multi-abi"

# smoelius: `get` works for non-standard variable names like `INPUT_CORPUS-DIR`.
get() {
    env | sed -n "s/^$1=\(.*\)/\1/;T;p"
}

CMD=(echidna-test "$INPUT_FILES")

for OPTION in $OPTIONS; do
    NAME=INPUT_"${OPTION^^}"
    VALUE="$(get "$NAME")"
    if [[ -n "$VALUE" ]]; then
        CMD+=(--"$OPTION" "$VALUE")
    fi
done

for SWITCH in $SWITCHES; do
    NAME=INPUT_"${SWITCH^^}"
    VALUE="$(get "$NAME")"
    if [[ -n "$VALUE" ]]; then
        CMD+=(--"$SWITCH")
    fi
done

echo "${CMD[@]}" >&2

if [[ -n "$(get 'INPUT_NEGATE-EXIT-STATUS')" ]]; then
    ! "${CMD[@]}"
else
    "${CMD[@]}"
fi
