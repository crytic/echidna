contract C {
  bool state = true;

  function C(uint x) {
     require(x <= 1024);
  }

  function f(uint x, uint y, uint z) {
    require(x == 12);
    require(y == 8);
    require(z == 0);
    state = false;
    return; 
  }

  function echidna_state() returns (bool) {
    return state;
  }

}
