## TODO

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
