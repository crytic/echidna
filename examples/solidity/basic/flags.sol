contract Test {
  bool private flag0=true;
  bool private flag1=true;

  event S0(int x);
  event S1(int y);

  function set0(int val) public returns (bool){
    emit S0(val);
    if (val % 100 == 0) {flag0 = false;}
  }
  function set1(int val) public returns (bool){
    emit S1(val);
    if (val % 10 == 0 && !flag0) {flag1 = false;}
  }
  function echidna_alwaystrue() public returns (bool){
    return(true);
  }
  function echidna_revert_always() public returns (bool){
    revert();
  }
  function echidna_sometimesfalse() public returns (bool){
    return(flag1);
  }
}
