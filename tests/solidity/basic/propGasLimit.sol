contract propGasLimit {
  uint256 n = 0;

   function f (uint256 newn) public {
    n = newn;
  }

   function echidna_runForever() public returns (bool) {
    uint x = 0;
    for (uint256 i = 0; i < n; i++) {
      x += i;
    }
    return true;
  }
}
