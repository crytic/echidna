// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract FallbackTest {
    uint256 public fallbackCalled = 0;

    // Fallback function
    fallback() external {
        fallbackCalled++;
        // This should fail when fallback is called
        assert(fallbackCalled == 0);
    }
}

