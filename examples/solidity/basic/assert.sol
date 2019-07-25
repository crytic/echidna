contract Test {
  function set0(int val) public returns (bool){
    assert(val % 100 != 0);
  }
  function set1(int val) public returns (bool){
  }
  function echidna_true() public returns (bool){
    return(true);
  }
}
