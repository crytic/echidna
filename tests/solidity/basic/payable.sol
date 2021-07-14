contract C {

  uint state = 0;
  uint another_state = 0;
  function payable_function() public payable {
    if (msg.value == 129)
        state = msg.value;
  }

  function another_function(uint) public {
    another_state = 1;
  }

  function echidna_payable() public returns (bool) {
    return(state == 0);
  }
}
