pragma experimental ABIEncoderV2;
pragma solidity ^0.4.25;

contract Dynamic {
  struct Yolo { // Struct
    uint u;
    string s;
    address a;
  }

  bool cond = true;

  function yolo(Yolo memory y) public returns (bool) {
    if (keccak256(y.s) == keccak256("yolo")) {
      cond = false;
    }
    return(true);
  }

  function echidna_test() public returns (bool) {
    return(cond);
  }
}
