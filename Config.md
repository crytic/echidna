# Configuration options

Echidna's CLI can be used to choose the contract to test and load a
configuration file.

```
$ echidna-test contract.sol --contract TEST --config config.yaml
```

The configuration file allows users to choose EVM and test generation
parameters. An example of a complete and annotated config file with the default
options can be found at
[examples/solidity/basic/default.yaml](examples/solidity/basic/default.yaml).

## Options in the configuration file

Echidna allows a variety of config options in a yaml file (e.g `config.yaml`):

* `testLimit`
  * Type: Int
  * Default: `50000`
  * Description: Number of sequences of transactions to generate during testing.  

* `seqLen` 
  * Type: Int
  * Default: `100`
  * Description: Number of transactions to generate during testing.

* `shrinkLimit` 
  * Type: Int
  * Default: `5000`
  * Description: Number of tries to attempt to shrink a failing sequence of transactions.

* `contractAddr`
  * Type:  Address
  * Default: `"0x00a329c0648769a73afac7f9381e08fb43dbea72"`
  * Description: Address to deploy the contract to test.

* `coverage`
  * Type:  Bool
  * Default: `true`
  * Description: Enable the use of coverage-guided fuzzing and corpus collection.

* `corpusDir`
  * Type:  String
  * Default: `null`
  * Description: Directory to save the corpus collected (requires coverage enabled).

* `deployer`
  * Type:  Address
  * Default: `"0x00a329c0648769a73afac7f9381e08fb43dbea70"`
  * Description: Address of the deployer of the contract to test.

* `sender`
  * Type: [Address]
  * Default: `["0x00a329c0648769a73afac7f9381e08fb43dbea70"]`
  * Description: List of addresses to (randomly) use during for the transactions sent during testing.

* `psender`
  * Type: Address
  * Default: `"0x00a329c0648769a73afac7f9381e08fb43dbea70"`
  * Description: Address of the sender of the property to test.

* `prefix`
  * Type: String 
  * Default: `"echidna_"`
  * Description: Prefix of the function names used as properties in the contract to test. 

* `propMaxGas`
  * Type: Int 
  * Default: `12500000` (current max gas per block)
  * Description: Maximum amount of gas to consume when running function properties.

* `testMaxGas`
  * Type: Int 
  * Default: `12500000` (current max gas per block)
  * Description: Maximum amount of gas to consume when running random transactions.

* `maxGasprice`
  * Type: Int 
  * Default: `0` 
  * Description: Maximum amount of gas price to randomly use in transactions. Do not change it unless you absolutely need it.

* `maxTimeDelay`
  * Type: Int 
  * Default: `604800`  (one week) 
  * Description: Maximum amount of seconds of delay between transactions.

* `maxBlockDelay`
  * Type: Int 
  * Default: `60480`   
  * Description: Maximum amount of block numbers between transactions.
 
* `solcArgs`
  * Type: [String] 
  * Default: `[]`
  * Description: Additional arguments to use in `solc` for the compilation of the contract to test. 

* `cryticArgs`
  * Type: [String] 
  * Default: `[]`
  * Description: Additional arguments to use in `crytic-compile` for the compilation of the contract to test. 

* `quiet`
  * Type: Bool
  * Default: `False`
  * Description: Hide `solc` stderr output and additional information during the testing.

* `dashboard`
  * Type: Bool
  * Default: `True`
  * Description: Show the ncurses dashboard with real-time information on the properties to test during the fuzzing campaign.

* `initialBalance`
  * Type: Int
  * Default: `0xffffffff`
  * Description: Initial Ether balance of `deployer` and each of the `sender` accounts.

* `format`
  * Type: String
  * Default: `"text"`
  * Description: Select an UI to show the results of each test. Either`"text", `"json"` or`"none"`.

Echidna supports three different output drivers. There is the default `text`
driver, a `json` driver, and a `none` driver, which should suppress all
`stdout` output. 

## JSON output

The JSON driver reports the overall campaign as follows.

```json
Campaign = {
  "success"      : bool,
  "error"        : string?,
  "tests"        : [Test],
  "seed"         : number,
  "coverage"     : Coverage,
  "gas_info"     : [GasInfo]
}
Test = {
  "contract"     : string,
  "name"         : string,
  "status"       : string,
  "error"        : string?,
  "testType"     : string,
  "transactions" : [Transaction]?
}
Transaction = {
  "contract"     : string,
  "function"     : string,
  "arguments"    : [string]?,
  "gas"          : number,
  "gasprice"     : number
}
```

`Coverage` is a dict describing certain coverage increasing calls.
Each `GasInfo` entry is a tuple that describes how maximal
gas usage was achieved, and also not too important. These interfaces are
subject to change to be slightly more user friendly at a later date. `testType`
will either be `property` or `assertion`, and `status` always takes on either
`fuzzing`, `shrinking`, `solved`, `passed`, or `error`.
