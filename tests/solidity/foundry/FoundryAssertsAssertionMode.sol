// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "forge-std/Test.sol";

// Test that Foundry's assertX functions trigger failures in assertion mode.
// In assertion mode, ALL functions are checked (no test_ prefix needed).
contract FoundryAssertsAssertionModeTest is Test {
    // This assertion can be broken when x > 100
    function check_assert_true(uint256 x) public pure {
        assertTrue(x <= 100);
    }

    // This assertion can be broken when x != y
    function check_assert_eq(uint256 x, uint256 y) public pure {
        assertEq(x, y);
    }

    // This assertion can be broken when x <= y
    function check_assert_gt(uint256 x, uint256 y) public pure {
        assertGt(x, y);
    }

    // This function should never fail
    function safe_function(uint256 x) public pure {
        uint256 y = x + 0;
        require(y == x);
    }
}
