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
    // This assertion can be broken when x > 100
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

contract AssertEqDecimalTest is Test {
    // This assertion can be broken when x != y
    function test_assert_eq_decimal(uint256 x, uint256 y) public pure {
        assertEqDecimal(x, y, 1);
    }
}

contract AssertNotEqDecimalTest is Test {
    // This assertion can be broken when x == y
    function test_assert_not_eq_decimal(uint256 x, uint256 y) public pure {
        assertNotEqDecimal(x, y, 1);
    }
}

contract AssertLtTest is Test {
    // This assertion can be broken when x >= y
    function test_assert_lt(uint256 x, uint256 y) public pure {
        assertLt(x, y);
    }
}

contract AssertGtTest is Test {
    // This assertion can be broken when x <= y
    function test_assert_gt(uint256 x, uint256 y) public pure {
        assertGt(x, y);
    }
}

contract AssertLtDecimalTest is Test {
    // This assertion can be broken when x >= y
    function test_assert_lt_decimal(uint256 x, uint256 y) public pure {
        assertLtDecimal(x, y, 1);
    }
}

contract AssertGtDecimalTest is Test {
    // This assertion can be broken when x <= y
    function test_assert_gt_decimal(uint256 x, uint256 y) public pure {
        assertGtDecimal(x, y, 1);
    }
}

contract AssertLeTest is Test {
    // This assertion can be broken when x > y
    function test_assert_le(uint256 x, uint256 y) public pure {
        assertLe(x, y);
    }
}

contract AssertGeTest is Test {
    // This assertion can be broken when x < y
    function test_assert_ge(uint256 x, uint256 y) public pure {
        assertGe(x, y);
    }
}

contract AssertLeDecimalTest is Test {
    // This assertion can be broken when x > y
    function test_assert_le_decimal(uint256 x, uint256 y) public pure {
        assertLeDecimal(x, y, 1);
    }
}

contract AssertGeDecimalTest is Test {
    // This assertion can be broken when x < y
    function test_assert_ge_decimal(uint256 x, uint256 y) public pure {
        assertGeDecimal(x, y, 1);
    }
}

contract AssertApproxEqAbsTest is Test {
    // This assertion can be broken when |x - y| > 1
    function test_assert_approx_eq_abs(uint256 x, uint256 y) public pure {
        // Allowing 1 unit of difference between x and y
        assertApproxEqAbs(x, y, 1);
    }
}

contract AssertApproxEqAbsDecimalTest is Test {
    // This assertion can be broken when |x - y| > 100 (with 1 decimal
    // precision)
    function test_assert_approx_eq_abs_decimal(uint256 x, uint256 y) public pure {
        assertApproxEqAbsDecimal(x, y, 100, 1);
    }
}

contract AssertApproxEqRelTest is Test {
    // This assertion can be broken when percentage delta > 1%
    // Note: 1e18 = 100%, so 0.01e18 = 1%
    function test_assert_approx_eq_rel(uint256 x, uint256 y) public pure {
        assertApproxEqRel(x, y, 0.01e18); // 1% tolerance
    }
}

contract AssertApproxEqRelDecimalTest is Test {
    // This assertion can be broken when percentage delta > 1% (formatted with
    // 1 decimal precision)
    // Note: 1e18 = 100%, so 0.01e18 = 1%
    function test_assert_approx_eq_rel_decimal(uint256 x, uint256 y) public pure {
        assertApproxEqRelDecimal(x, y, 0.01e18, 1); // 1% tolerance
    }
}

contract RevertTest is Test {
    // Explicit reverts should be detected as test failures
    function test_revert_is_failure(uint256 x) public pure {
        if (x > 100) {
            revert("Value too large");
        }
    }
}