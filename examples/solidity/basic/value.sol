contract C {
  constructor() payable public {
  }

  bool state = true;
  function f() payable public {
    if (msg.value > 0xffff)
      state = false;
  }
  function echidna_value() public returns (bool) { 
      return state;
  }

}
