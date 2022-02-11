pragma experimental ABIEncoderV2;

contract Dynamic {
  struct Yolo { // Struct
    uint u;
    string s;
    address a;
  }

  bool cond = true;

  function yolo(Yolo memory y) public returns (bool) {
    if (keccak256(abi.encodePacked(y.s)) == keccak256(abi.encodePacked("yolo"))) {
      cond = false;
    }
    return(true);
  }

  function echidna_test() public returns (bool) {
    return(cond);
  }
}
