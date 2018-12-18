contract C {
  bool state1 = false;
  bool state2 = false;
  bool state3 = false;
  
  function f(uint x) {
    require(x == 12);
    state1 = true;
  }

  function g(uint y) {
    require(state1);
    require(y == 8);
    state2 = true;    
  }
  
  function h(uint z) {
    require(state2);
    require(z == 0);
    state3 = true;
  }
  
  function ul(uint) {
    uint x = 0;
    return;
  }
  
  function echidna_state3() returns (bool) {
    return (!state3);
  }

}
