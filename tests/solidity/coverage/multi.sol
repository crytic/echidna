contract C {
  bool state1 = false;
  bool state2 = false;
  bool state3 = false;
  
  function f(uint x) public {
    require(x == 12);
    state1 = true;
  }

  function g(uint y) public {
    require(state1);
    require(y == 8);
    state2 = true;    
  }
  
  function h(uint z) public {
    require(state2);
    //require(z == 0);
    state3 = true;
  }
  
  function ul(uint) public {
    uint x = 0;
    return;
  }
  
  function echidna_state3() public returns (bool) {
    return (!state3);
  }

}
