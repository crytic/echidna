// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "./SimpleToken.sol";

/// The contract the MCP test campaign runs against.
///
/// It is here to give the tools something to report on rather than to find a
/// bug: a handful of functions to inject, sample and replay, one of which
/// reverts or succeeds depending on its arguments, and properties that hold so
/// the campaign keeps running for as long as the test session needs it.
contract EchidnaMCPTest {
    SimpleToken public token;
    uint256 constant INITIAL_SUPPLY = 1000000 * 10**18;

    constructor() {
        token = new SimpleToken(INITIAL_SUPPLY);
    }

    /// Succeeds for an amount this contract still holds, reverts otherwise --
    /// which is most of the time, the amount being a random uint256.
    function transferTokens(address to, uint256 amount) public {
        token.transfer(to, amount);
    }

    function approveSpender(address spender, uint256 amount) public {
        token.approve(spender, amount);
    }

    /// Succeeds for any recipient but the zero address.
    function mintTokens(address to, uint256 amount) public {
        token.mint(to, amount);
    }

    function tokenAddress() public view returns (address) {
        return address(token);
    }

    /// Nothing burns, so the supply only ever grows.
    function echidna_supply_never_shrinks() public view returns (bool) {
        return token.totalSupply() >= INITIAL_SUPPLY;
    }

    /// No account can hold more than was ever issued.
    function echidna_balance_within_supply() public view returns (bool) {
        return token.balanceOf(address(this)) <= token.totalSupply();
    }
}
