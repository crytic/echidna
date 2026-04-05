// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

// Tests that excludeViewPure does NOT exclude prefixed view/pure functions
// in property mode, since those are property tests.
contract ViewPureProperty {
    uint256 public counter;

    function increment() external {
        counter++;
    }

    // view property — must NOT be excluded even with excludeViewPure: true
    function echidna_counter_small() public view returns (bool) {
        return counter < 10;
    }

    // non-prefixed view — should be excluded
    function getCounter() external view returns (uint256) {
        return counter;
    }

    // non-prefixed pure — should be excluded
    function getConst() external pure returns (uint256) {
        return 42;
    }
}
