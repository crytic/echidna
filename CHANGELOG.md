## 2.2.4

* Initial TLOAD/TSTORE support (#1286)
* Initial symbolic execution implementation (experimental, #1216, #1251, #1285)
* New panel toggles in the UI (#1197)
* New gas/s metric (#1279)
* Improved logging (#1202, #1271, #1273, #1274, #1283, #1258, #1269)
* Performance improvements with multiple workers (#1228)
* Shrinking improvements (#1196, #1250, #1280)
* Improved configuration options (#1200, #1227, #1251)

## 2.2.3

* feat: add CLI commands for RPC URL and block number (#1194)
* Speed up shrinking (#1192)
* Update hevm to 0.53.0 (#1189)
* Fix faulty corpus transaction detection (#1184)
* Display contract names in UI (#1181)
* Save traces for every transaction in reproducer (#1180)
* Validate corpus while replaying (#1177)
* Refactors (#1174, #1178)

## 2.2.2

* Save corpus and reproducers continuously (#1167)
* Deliver status information using server-sent events (#1131)
* Performance improvements for coverage collection (#1160)
* Make slither optional (#1159)
* Rich trace printing (#1157)
* Static builds and release workflow (#1133)
* Re-enables using slither for vyper files (#1108)
* Dependency updates (#1153, #1096, #1154)

## 2.2.1

* Shanghai fork support with hevm 0.51.3 (#1090)
* Fixed coverage collection for delegatecalls (#1083)
* Added events to JSON output (#1069)
* Changed event sequence to be displayed on new lines (#1079)
* Improved "No tests found in ABI" error message (#1077)
* Refactored code (#1070, #1074)

## 2.2.0

* Multicore fuzzing (#963, #1033, #1026, #1035)
* Lcov format support (#1029)
* Experimental power number generator for uints (#892)
* Coverage collection optimization (#1003, #1041)
* On-chain fuzzing improvements (#1017)
* Refactored code (#1020, #1021)
* Updated dependencies (#1022, #1023)
* UI improvements (#1031, #1032, #1034, #1040)
* Readme improvements (#1019)

## 2.1.1

* Added missing space in ProcessorNotFound message (#977)
* Added measurement and log of external actions (#988)
* Avoid using cheat codes address to form fuzzing call sequences (#993)
* Implemented invariant testing from foundry (#989)
* hevm upgraded to 0.50.4 (#986)
* Cleaned and improved codebase (#990, #994, #995, #997)
* Make frequently modified fields strict (#1000)
* Force corpus evaluation (#1002)
* Text/headless UI was improved (#991, #1006, #1007, #1009)

## 2.1.0

* External contracts are now automatically fetched from Ethereum nodes during fuzzing (#927, #971)
* Added support for the FFI cheatcode (#750)
* Renamed multi-abi mode to allContracts, multi-abi still works but will be removed in future (#934)
* Fixed method filtering in multi-abi mode (#950, #954)
* Fixed config parsing for too large values (#935)
* Renamed echidna-test binary to echidna (#826)
* Added scrollbar to the UI (#915)
* Removed generation of negative seeds (#933)
* Added crash display to the UI (#944)
* Added human-friendly errors for panic codes (#965)
* Refactored code (#903, #906, #908, #924, #925, #928, #946, #956, #966, #968)
* Added limited Windows build (#943)
* Updated dependencies (#942, #948)
* Build and CI improvements (#912, #914, #917, #952, #967)

## 2.0.5

* Optimized constant generation (#898, #900)
* Fixed how address are displayed in events (#891)
* Update hevm to 0.50 (#884, #894, #896, #897, #901)
* Added saving and loading of reproducers for every test (#858)
* Added events and revert reasons for any failure in the constructor (#871)
* Fixed uninitialized sender addresses from etheno transactions (#823)
* Fixed crash when minimizing inputs during optimization tests (#837)
* Refactored code and removed useless dependencies (#856, #857, #874, #878, #895, #903)

## 2.0.4

* Added colored html for coverage output code (#816)
* Fixed crash when parsing solc versions (#835)
* Fixed long transactions and event lines in UI (#832)
* Added Homebrew installation instructions (#848)
* Moved all nix stuff to flake and use nix-bundle-exe for macOS release (#851)
* Updated codebase to GHC 9.0.2 (#846)
* Refactored code and removed useless dependencies (#854, #853, #829, #827, #828)

## 2.0.3

* Clean up Docker containers (#706)
* Avoid resetting accounts if there is a deployed contract (#795)
* Fixed decoding non-utf8 strings from slither printer (#799)
* Fixed generation and mutation of extreme signed integers (#791)
* Removed fallback from signature map when it is not defined (#772)

## 2.0.2

* Improved shrinking of dynamic arrays (#775)
* Added support for deployment of certain contracts or bytecode in specific addresses (#758)
* Fixed git attribute to support building docker containers in Windows (#773)
* Fixed crash when the EVM execution triggers more than one query (#760)
* Added support for detection and handling of ancient solc versions (#675)
* Added explicit static flag and removed pthread one from ghc options (#768)

## 2.0.1

* Optimized stateless mutators (#747)
* Expanded and improved command-line help (#741)
* Added dapptest support: compatibility mode to run foundry and dapptool fuzz tests (#733, #745)
* Generate more values closer to the maximum (#736)
* Fix TERMINFO path for Nix release builds (#731)
* Mitigate large memory consumption when replaying corpus (#725)
* Fix --shrink-limit to change shrink limit instead of test limit (#728)
* Correctly show lines with multiple types of coverage
* Restored nix support (#717, #743)

## 2.0.0

* Refactored test internal data structures and code
* Refactored unit test code and moved the related files to the `tests` directory
* Added support to show events and custom errors when a property/assertion fails
* Added support for catching assertion failure in Solidity 0.8.x
* Added two new testing mode: optimization and overflow (only in Solidity 0.8.x)
* Added optional checks for contract destruction
* Added `testMode` option and removed related flags
* Simplified contract deployer and property sender addresses to be easier to read
* Updated hevm to 0.49.0

## 1.7.3

* Removed old compilation artifacts before starting a new fuzzing campaign (#697)
* Fixed incorrect function filtering when using assertion mode (#695)
* Improved handling of negative constants (#683)
* Updated hevm to 0.48.0 (#691)
* Removed nix workaround for Slither (#680)
* Fixed source printing when some lines are not covered (#678)

## 1.7.2

* Fixed check-asserts and multi-abi cli switches (#665)
* Updated to hevm 0.4.6 (#660)
* Support for loading multiple files with compiled contracts from hardhat/brownie (#659)

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
* Allow to control corpus mutation constants using a configuration file (#373)

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
* Early termination on property failure (#323)
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
