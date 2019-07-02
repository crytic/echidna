contract smallValues {
  uint256 n = 0;
  
  function f (uint256 newn) public {
    n = newn;
  }
  
  function echidna_findSmall() public returns (bool) {
    return ((n - 256) != 31);
  }
}
