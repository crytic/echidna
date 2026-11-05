// SPDX-License-Identifier: MIT
pragma solidity >=0.6.9;

contract PropertyVerifyTest {
  uint256 public x;
  uint256 public y;
  uint256 public lockedUntil;

  function set_x(uint256 _x) public {
    require(_x >= 5, "too low");
    require(_x <= 20, "too high");
    x = _x;
  }

  function set_y(uint256 _y) public {
    require(_y % 2 == 0, "must be even");
    require(_y > 0, "must be positive");
    y = _y;
  }

  function lock(uint256 duration) public {
    require(duration > 0 && duration <= 365 days, "bad duration");
    lockedUntil = block.timestamp + duration;
  }

  function echidna_x_is_not_ten() public returns (bool) {
    return x != 10;
  }

  function echidna_x_is_not_one() public returns (bool) {
    return x != 1;
  }

  function echidna_y_below_40() public returns (bool) {
    return y < 40;
  }

  function echidna_locked_before_1000() public returns (bool) {
    return lockedUntil <= 1000;
  }
}
