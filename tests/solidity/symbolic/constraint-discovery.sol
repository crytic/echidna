pragma solidity ^0.8.13;

contract ConstraintLimitStore {
  uint256 public limit = 0x888;

  function lower() external {
    limit = 0x123;
  }
}

contract ConstraintDiscoveryTest {
  ConstraintLimitStore internal limits;

  constructor() {
    limits = new ConstraintLimitStore();
  }

  function lowerLimit() external {
    limits.lower();
  }

  function constrained(uint256 x1, uint256, uint256 x3, uint256 x4) external {
    require(x3 >= 0x99);
    require(x4 <= limits.limit());
    require(x1 + 1 >= 0x20);
  }

  function echidna_bounds_campaign_runs() external pure returns (bool) {
    return true;
  }
}
