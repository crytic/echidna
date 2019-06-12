contract Constants {
  bool found = false;

  function find(int i) public {
    if (i == 1447) {found = true;}
    if (i == 133700000000) {found = false;}
  }

  function echidna_found() public view returns (bool) {
    return(!found);
  }
}
