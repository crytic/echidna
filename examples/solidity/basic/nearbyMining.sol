contract nearbyMining {
  uint256 n = 0;
  
  function f (uint256 newn) public {
    n = newn;
  }
  
  function echidna_findNearby() public returns (bool) {
    if (n <= 181888880989308019) {
      return true;
    }
    if (n >= 181888880989308021) {
      return true;
    }
    return false;
  }
}
