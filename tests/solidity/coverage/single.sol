contract C {
  bool state = true;
  function f(uint x, uint y, uint z) public {
    require(x == 42424242);
    require(y == 8);
    state = false;
    return; 
  }

  function echidna_state() public returns (bool) {
    return state;
  }

}
