contract C {

  function killed_me(address payable x) public {
      selfdestruct(x);
  }

  function echidna_still_alive() public returns (bool) {
    return true;
  }

}
