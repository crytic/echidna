contract C {

  uint state = 0;
  function payable_function() public payable {
    if (msg.value > 128)
        state = msg.value;
  }

  function another_payable_function(uint, address, address[] memory) public payable {}

  function echidna_test() public returns (bool) {
    return(state == 0);
  }
}
