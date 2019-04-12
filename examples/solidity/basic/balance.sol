contract C {
  constructor() public {
      //msg.sender.transfer(0);
  }

  function f() public {}
  function echidna_balance() public returns (bool) { 
      return address(msg.sender) == address(0x1) && msg.sender.balance == 42; 
  }

}
