contract Constants {
  bool found = false;
  bool found_large = false;

  function find(int i) public {
    if (i == 1447) { found = true; }
    if (i == 133700000000) { found = false; }
    if (i == 11234567890123456789012345678901234560) { found_large = true; }
  }

  function echidna_found() public view returns (bool) {
    return(!found);
  }

  function echidna_found_large() public view returns (bool) {
    return(!found_large);
  }

}
