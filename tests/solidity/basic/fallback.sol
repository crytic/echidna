contract C {
  bool state = true;

  function () external payable {
    state = false;
  }

  function f() public { return; }

  function echidna_fallback() public returns (bool) { 
    return state; 
  }
}
