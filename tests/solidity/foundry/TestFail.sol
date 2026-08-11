// SPDX-License-Identifier: MIT

// Foundry "testFail" functions are expected to revert, so their outcome is
// inverted: they fail when the call succeeds. No forge-std needed here, the
// convention is purely about the function name.
contract TestFailTest {
    function IS_TEST() public pure returns (bool) {
        return true;
    }

    // Always reverts, so this test passes
    function testFail_always_reverts(uint256) public pure {
        revert("expected revert");
    }

    // Succeeds for any input above 100, breaking the expectation that a
    // "testFail" function always reverts
    function testFail_sometimes_reverts(uint256 x) public pure {
        require(x > 100, "expected revert");
    }
}
