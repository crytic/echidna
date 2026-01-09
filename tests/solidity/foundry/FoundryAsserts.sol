// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "forge-std/Test.sol";

// Test contracts for Foundry's assertX functions detection

contract AssertTrueTest is Test {
    // This assertion can be broken when x > 100
    function test_assert_true(uint256 x) public pure {
        assertTrue(x <= 100);
    }
}

contract AssertFalseTest is Test {
    // This assertion can be broken when x <= 100
    function test_assert_false(uint256 x) public pure {
        assertFalse(x > 100);
    }
}

contract AssertEqTest is Test {
    // This assertion can be broken when x != y
    function test_assert_eq(uint256 x, uint256 y) public pure {
        assertEq(x, y);
    }
}

contract AssertNotEqTest is Test {
    // This assertion can be broken when x == y
    function test_assert_not_eq(uint256 x, uint256 y) public pure {
        assertNotEq(x, y);
    }
}