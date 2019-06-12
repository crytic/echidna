contract ConstantsBytes32 {
  bool found = false;

  function find(bytes32 s) public {
    if (s == "test") {found = true;}
  }

  function echidna_found32() public view returns (bool) {
    return(!found);
  }
}
