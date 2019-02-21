pragma solidity ^0.4.24;

contract Constants {
  bool found = false;

  function find(int i) public {
    if (i == 1337) {found = true;}
  }

  function echidna_found() public view returns (bool) {
    return(!found);
  }
}
