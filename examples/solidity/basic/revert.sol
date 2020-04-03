contract C {

  int private state = 0;
  function f(int x, address y, address z) public {
    require(z != address(0x0));
    state = x;
  }

  function echidna_fails_on_revert() public returns (bool) {
    if (state < 0)
      revert();
    return true;
  }
}
