pragma solidity ^0.4.25;

contract Dynamic {
  bool cond = true;

  function yolo(uint256 x, string memory s, uint256 y) public returns (bool) {
    if (keccak256(s) == keccak256("yolo")) {
      cond = false;
    }
    return(true);
  }

  function echidna_test() public returns (bool) {
    return(cond);
  }
}
