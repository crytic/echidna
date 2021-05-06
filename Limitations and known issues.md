# Limitations and known issues

EVM emulation and testing is hard. Echidna has a number of limitations in the latest release. Some of these are inherited from [hevm](https://github.com/dapphub/dapptools/tree/master/src/hevm) while some are results from design/performance decisions or simply bugs in our code. We list them here including their corresponding issue and the status ("wont fix", "in review", "fixed"). Issues that are "fixed" are expected to be included in the next Echidna release.

| Description |  Issue   | Status   |  
| :--- |     :---:              |         :---:   |
| Debug information can be insufficient | [#656](https://github.com/crytic/echidna/issues/656) | *in review* |
| Vyper support is limited | [#652](https://github.com/crytic/echidna/issues/652) | *wont fix* |
| Limited library support for testing | [#651](https://github.com/crytic/echidna/issues/651) | *wont fix* |
| If the contract is not properly linked, Echidna will crash | [#514](https://github.com/crytic/echidna/issues/514) | *in review* | 
| Assertions are not detected in internal transactions | [#601](https://github.com/crytic/echidna/issues/601) | *in review* |
| Some mutations generate inputs outside their expected ABI range | [#654](https://github.com/crytic/echidna/issues/654) | *fixed* |
| Uninitialized addresses can produce unexpected reverts | [#653](https://github.com/crytic/echidna/issues/653) | *fixed* | 
| Value generation can fail in multi-abi mode, since the function hash is not precise enough | [#579](https://github.com/crytic/echidna/issues/579) | *in review*|
