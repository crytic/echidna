contract Constants {
  bool found = false;
  uint dummy;

  function seed() public returns (int) {
    // dummy state write so Slither doesn't report seed() as a constant
    // function, which would make Echidna call it too rarely to reliably
    // pick up its return value
    dummy = 1;
    int mystery = 13337;
    return (1337 + mystery);
  }

  function find(int i) public {
    int mystery = 13337;
    if (i == 1337 + mystery) {found = true;}
  }

  function echidna_found() public view returns (bool) {
    return(!found);
  }
}
