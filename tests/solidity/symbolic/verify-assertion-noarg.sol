// SPDX-License-Identifier: MIT
pragma solidity >=0.6.9;

// Test contract for two-phase assertion mode: no-arg assertion functions
// that can only be falsified via state set by other functions.
contract AssertionNoArgTest {
    uint256 public x;
    uint256 public y;

    function set_x(uint256 _x) public {
        require(_x >= 5, "too low");
        require(_x <= 20, "too high");
        x = _x;
    }

    function set_y(uint256 _y) public {
        require(_y > 0, "must be positive");
        y = _y;
    }

    // No-arg assertion: should be falsified when x == 10
    function check_x_not_ten() public view {
        assert(x != 10);
    }

    // No-arg assertion: should be falsified when y >= 100
    function check_y_below_100() public view {
        assert(y < 100);
    }

    // No-arg assertion: should never be falsified (x can't be 1 due to require >= 5)
    function check_x_not_one() public view {
        assert(x != 1);
    }
}
