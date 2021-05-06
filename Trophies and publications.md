# Echidna Trophies

## Security Issues

The following lists security vulnerabilities that were found by Echidna. If you found a security vulnerability using our tool, please submit a PR with the relevant information.

| Project | Vulnerability | Date |
|--|--|--|
[0x Protocol](https://github.com/trailofbits/publications/blob/master/reviews/0x-protocol.pdf) | If an order cannot be filled, then it cannot be canceled | Oct 2019
[0x Protocol](https://github.com/trailofbits/publications/blob/master/reviews/0x-protocol.pdf) | If an order can be partially filled with zero, then it can be partially filled with one token | Oct 2019
[0x Protocol](https://github.com/trailofbits/publications/blob/master/reviews/0x-protocol.pdf) | The cobbdouglas function does not revert when valid input parameters are used | Oct 2019
[Balancer Core](https://github.com/trailofbits/publications/blob/master/reviews/BalancerCore.pdf) | An attacker cannot steal assets from a public pool | Jan 2020
[Balancer Core](https://github.com/trailofbits/publications/blob/master/reviews/BalancerCore.pdf) | An attacker cannot generate free pool tokens with joinPool | Jan 2020
[Balancer Core](https://github.com/trailofbits/publications/blob/master/reviews/BalancerCore.pdf) | Calling joinPool-exitPool does not lead to free pool tokens | Jan 2020
[Balancer Core](https://github.com/trailofbits/publications/blob/master/reviews/BalancerCore.pdf) |  Calling exitswapExternAmountOut does not lead to free assets | Jan 2020
[Yield Protocol](https://github.com/trailofbits/publications/blob/master/reviews/YieldProtocol.pdf) | Arithmetic computation for buying and selling tokens is imprecise | Aug 2020
[Origin Dollar](https://github.com/trailofbits/publications/blob/master/reviews/OriginDollar.pdf) | Users are allowed to transfer more tokens that they have | Nov 2020
[Origin Dollar](https://github.com/trailofbits/publications/blob/master/reviews/OriginDollar.pdf) | User balances can be larger than total supply | Nov 2020
[Liquity Dollar](https://github.com/trailofbits/publications/blob/master/reviews/Liquity.pdf) | Closing troves require to hold the full amount of LUSD minted | Dec 2020
[Liquity Dollar](https://github.com/trailofbits/publications/blob/master/reviews/Liquity.pdf) | Troves can be improperly removed | Dec 2020
[Liquity Dollar](https://github.com/trailofbits/publications/blob/master/reviews/Liquity.pdf) | Initial redeem can revert unexpectedly | Dec 2020
[Liquity Dollar](https://github.com/trailofbits/publications/blob/master/reviews/Liquity.pdf) | Redeem without redemptions might still return success | Dec 2020

## Research Examples

We can also use Echidna to reproduce a number of research examples from smart contract fuzzing papers to show how quickly it can find the solution:

| Source | Code 
|--|--
[Using automatic analysis tools with MakerDAO contracts](https://forum.openzeppelin.com/t/using-automatic-analysis-tools-with-makerdao-contracts/1021) | [SimpleDSChief](https://github.com/crytic/echidna/blob/master/examples/solidity/research/vera_dschief.sol)
[Integer precision bug in Sigma Prime](https://github.com/b-mueller/sabre#example-2-integer-precision-bug) | [VerifyFunWithNumbers](https://github.com/crytic/echidna/blob/master/examples/solidity/research/solcfuzz_funwithnumbers.sol)
[Learning to Fuzz from Symbolic Execution with Application to Smart Contracts](https://files.sri.inf.ethz.ch/website/papers/ccs19-ilf.pdf) | [Crowdsale](https://github.com/crytic/echidna/blob/master/examples/solidity/research/ilf_crowdsale.sol)
[Harvey: A Greybox Fuzzer for Smart Contracts](https://arxiv.org/abs/1905.06944) | [Foo](https://github.com/crytic/echidna/blob/master/examples/solidity/research/harvey_foo.sol), [Baz](https://github.com/crytic/echidna/blob/master/examples/solidity/research/harvey_baz.sol)

All these can be solved, from a few seconds to one or two minutes on a laptop computer.

# Publications

## Trail of Bits
- [Echidna: effective, usable, and fast fuzzing for smart contracts](https://github.com/trailofbits/publications/blob/master/papers/echidna_issta2020.pdf), Gustavo Grieco, Will Song, Artur Cygan, Josselin  Feist, Alex Groce - ISSTA '20

If you are using Echidna on an academic work, consider applying to the [Crytic $10k Research Prize](https://blog.trailofbits.com/2019/11/13/announcing-the-crytic-10k-research-prize/).
