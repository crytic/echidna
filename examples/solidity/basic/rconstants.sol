contract Constants {
  bool found = false;

  function seed() public returns (int) {
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
