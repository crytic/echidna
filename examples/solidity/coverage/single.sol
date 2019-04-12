contract C {
  bool state = true;
  function f(uint x, uint y, uint z) public {
    require(x == 12);
    require(y == 8);
    require(z == 0);
    state = false;
    return; 
  }

  function echidna_state() public returns (bool) {
    return state;
  }

}
