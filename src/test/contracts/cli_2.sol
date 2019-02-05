pragma solidity ^0.5;

contract Test {
  bool private flag0=true;
  bool private flag1=true;

  function set0(int val) public returns (bool){
    if (val % 10 == 0) {flag0 = false;}
  }
  function set1(int val) public returns (bool){
    if (val % 10 == 0 && flag0) {flag1 = false;}
  }
  function echidna_sometimesfalse() public returns (bool){
    return(flag0 || flag1);
  }
}
