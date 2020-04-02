contract C {

  uint state = 0;
  function f() public payable {
    if (msg.value > 128)
        state = msg.value;
  }

  function g() public payable {}

  function echidna_test() public returns (bool) {
    return(state == 0);
  }
}
