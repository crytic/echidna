contract C {
  bool value_found = false;
  constructor() payable public {
      msg.sender.transfer(0);
      msg.sender.transfer(123);
  }

  function f() public {}
  function echidna_balance() public returns (bool) { 
      return address(msg.sender) == address(0x42) && msg.sender.balance == (42+123) && address(this).balance == 0;
  }

  function echidna_balance_new() public returns (bool) { 
      return address(0xcafe).balance == 0;
  }

  function echidna_low_level_call() public returns (bool) {
      address(0xcafe).call("");
      return true;
  }

  function echidna_no_magic() public returns (bool) {
    if (value_found)
      return false;

    value_found = true;
    uint x = address(0x123).balance;
    return true;
  }

}
