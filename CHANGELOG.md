## TODO

## 1.7.1

* Tweaked mutators and improved test stability (#628)
* Documented known issues and limitations (#655)
* Automatically initialize addresses when used (#657)
* Improved coverage to count number of EVM frames (#624)
* Avoid mutations to generate inputs outside their expected ABI range (#650)
* Various small to run Vyper contracts (#645)
* Improved integer generation to use small values (#644)
* Added two CLI options: --corpus-dir and --check-asserts (#640)
* Refactored shrinkSeq to improve readability (#639)
* Fixed UI to fit long function calls (#635)
* Refactor coverage types and added corpus size in UI (#627)
* Fixed link to macOS binary in binaries.soliditylang.org (#629)
* Fixed default.nix to use 1.7.0 as version (#623)
* Refactored Test type (#622) 

## 1.7.0

* Refactored and improved etheno support to be more useful (#615) 
* Coverage filenames are not overwritten (#620)
* Refactored the mutator code (#618)
* More corpus and array mutations implemented (#372)
* Source coverage is printed after fuzzing campaign (#516)
* Nix improvements and fixes (#603, #604, #608, #612)
* Simplified slither information parsing (#543) 
* Enabled use of coverage by default (#605)
* Run echidna tests in parallel (#571)

## 1.6.1

* Use a sensible default value for `block.gaslimit` (#596)
* Use metadata to detect deployed contracts (#593)
* Semver integration for improving testing with different solc versions (#594)
* Added some performance improvement in property execution (#576)
* Fixed wait bug when shrinking (#584) 
* Added funwithnumber example from Sabre (#565)
* Improved function filtering to be more precise (#570)
* Small fixes in the macOS CI (#597), the README (#590) and Nix (#581) 

## 1.6.0
* Slither is now a required dependency.
* Made sure that <ESC> gets mapped correctly, fixing #474 (#503)
* Fix library timestamp/block delay issue (#510)
* Make stack limit exceeded a revert (#517)
* Add some default transaction constants (#532)
* Make large constants work better with dictionaries (#523)
* Fix "flanky" corpus tests (#537)
* HEVM updated to [150dddc67b6cbad75fd4ae5a689452892f55ea26] (0.42) (#511)
  * this may introduce a slight performance hit as everything is partially symbolic
* Improved shrinking and pretty printing (#518)
* Integrate slither results (#451)
* Disagreements about `MonadState` and `MonadReader` (#545, #513)
* Simplified a bunch of code (#548, #549, #513)
* Fix negative address bug (#552)
* Various Github Actions improvements (#527, #554)
* Allow to bypass EIP-170 and set up a custom max code size (#544)
 
## 1.5.1

* Fix timestamp and block delays having the initial timestamp/block added to them (#460, #469)
* Fix GitHub actions due to new macOS image (#468)
* Relax `package.yaml` constraints (#466)
* Fix `extcodesize` by creating contracts correctly (#453, #454, #465)
* Refactor unit tests (#457)
* Add seed to UI (#456)
* Set an initial timestamp and block number (#455)
* Better README (#450)
* Code simplifications (#448)
* Detect contract deployment failures (#447)
* DockerHub containers in addition to GitHub (#445)
* Brought back print reporting after exiting the UI (#441)
* Refactor `Tx` (#437)

## 1.5.0

* Switch to x.y.z versioning scheme
* Refactor campaign-related code (#384)
* Fixed hevm-related memory leak (#423)
* Fixed array mutation memory leak (#442)
* Benchmark mode to run without tests (#420, #409)
* Migrated to GitHub actions (#414, #402)
* Corpus mutation handling fixes (#395, #403)
* Improved JSON output and reworked UI handling to better handle interactive and non-interactive mode (#405)
* Use safe mutation parameters to favor purely random generation (#393)
* Show address in hex using "0x" (#386)
* Enable addresses shrinking (#385)
* Fixed coverage counting when a contract is deployed multiple times (#410)
* Prioritization to mutate each list of transactions from the corpus (#376, #395)
* Improved array generation using generic mutations (#357)
* Multiple coverage tracking according to the result of a transaction (#369)
* Allow to control corpus mutation constans using a configuration file (#373)

## 1.4.0.1
* HEVM updated to [af84e2ee0a0654fdaa91186384233cf1731ee7ce]

## 1.4.0.0

* Filtering functions either blacklisting or whitelisting functions to call during a fuzzing campaign (#341)
* Support to collect, save and load a corpus of transactions in JSON (#352, #353)
* Basic mutations based on collected corpus (#370)
* Use of `hlint` as Github action to verify new PR (#366)

## 1.3.0.0

* Gaining knowledge of contract-created contract addresses (#295)
* Improved `crytic-compile` error handling when missing (#335)
* Support testing from arbitrary blockchain state via etheno (#333)
* Some shrinking improvements (#336)
* UI improvements (#339)
* UI updates are now threaded separately from fuzzing campaign (#345)
* `Tx` rework to unify `Echidna.RPC` and `Transaction` (#346)
* Adjustment of testsuite parameters (#347)
* Multi-ABI support by fuzzing all known ABIs (#344)
* Fix indefinite `MVar` blocking (#350)
* AddressArrayExample (#348)
* `Tx` JSON conversions (#352)
* Smaller docker container, multi-stage build (#349)
* Gas worst case estimation (#355)

## 1.2.0.0

* Warn on unused config keys (#301)
* `default.yaml` kept up to date via CI checks (#301)
* Improved shrinking (#310)
* Solidity ABIv2 support (#284, #303, #317)
* Early termination on property falure (#323)
* Timeouts (#320)
* HEVM updated to [2cc059b49cae613025b925f0273b906e25484b68]
* Dictionary control (#312, #321)
* Gas price fuzzing (#307)
* Coverage bug fixed (#300)
* Contract creation bug fixed (#293)
* New startup message (#290)
* Add `--version` flag (#285)
* Support time/block delays (#282)
* Support `crytic` arguments (#280)
* Detect assertion failure (#276)
* Use return values as constants (#262)
* Reproducible testing with seeds (#254)

## 1.0.0.0

* Initial stable release

[2cc059b49cae613025b925f0273b906e25484b68]: https://github.com/dapphub/dapptools/tree/2cc059b49cae613025b925f0273b906e25484b68
[af84e2ee0a0654fdaa91186384233cf1731ee7ce]: https://github.com/dapphub/dapptools/tree/af84e2ee0a0654fdaa91186384233cf1731ee7ce
[150dddc67b6cbad75fd4ae5a689452892f55ea26]: https://github.com/dapphub/dapptools/tree/150dddc67b6cbad75fd4ae5a689452892f55ea26
