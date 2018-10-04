contract C {

  uint state = 0;
  function f() payable {
    if (msg.value > 128)
        state = msg.value;
  }

  function echidna_test() returns (bool) {
    return(state == 0);
  }
}
