// SPDX-License-Identifier: MIT

// Simple stateless contract with an assertion that can be broken
contract StatelessBuggy {
    // This assertion can be broken when x > 100
    function checkValue(uint256 x) public pure {
        assert(x <= 100);
    }
}
