contract C {

  uint private state = 0;
  function f(uint x) public {
    state = x;
  }

  function killed_if() {
    require(state > 256);
    suicide(0x0);
  }

  function echidna_still_alive() public returns (bool) {
    return true;
  }

  function echidna_other_prop() public returns (bool) {
    return true;
  }

}
