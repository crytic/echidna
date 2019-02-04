# simple
echidna-test basic/flags.sol
echidna-test basic/revert.sol
echidna-test basic/killed.sol
echidna-test basic/log.sol

# tokens
echidna-test tokens/bad_erc20.sol --config tokens/bad_erc20_config.yaml TEST
echidna-test tokens/missing_constructor.sol --config tokens/missing_constructor_config.yaml TEST

# coverage
echidna-test coverage/harvey.sol --config coverage/harvey.yaml
echidna-test coverage/simple.sol --config coverage/config.yaml
echidna-test coverage/multi.sol --config coverage/config.yaml
