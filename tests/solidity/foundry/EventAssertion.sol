// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

// Test that event-based assertion failures generate Foundry reproducers.
// In assertion mode, emitting AssertionFailed() is detected as a failure.
contract EventAssertion {
    event AssertionFailed();

    uint256 public counter;

    function inc() external {
        counter++;
        if (counter > 3) {
            emit AssertionFailed();
        }
    }

    function dummy() external {}
}
