contract C {
  bool private state = true;
  function f(int x) returns (uint) {  
    if (x == 424242)
       state = false;
    return 0; 
  }
  function echidna_test() returns (bool) { return state; }
}
