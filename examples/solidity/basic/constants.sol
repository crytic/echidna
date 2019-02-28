contract Constants {
  bool found = false;

  function find(int i) public {
    if (i == 1337) {found = true;}
  }

  function find2(address a) public {
    if (a == address(0x123)) {found = true;}
  }

  function find3(string memory s) public {
    if (keccak256(bytes(s)) == keccak256("test")) {found = true;}
  }

  function echidna_found() public view returns (bool) {
    return(!found);
  }
}
