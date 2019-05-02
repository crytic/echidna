contract C {
  constructor() public {
      msg.sender.transfer(0); // this contract has no ether, but this call to transfer should not fail
  }

  function f() public {}
  function echidna_balance() public returns (bool) { 
      return address(msg.sender) == address(0x42) && msg.sender.balance == 42; 
  }

}
