contract smallValues {
  int256 n = 0;
  
  function f (uint256 newn) public {
    n = int256(newn);
  }
  
  function echidna_findSmall() public returns (bool) {
    return ((n - 256) != 31);
  }
}
