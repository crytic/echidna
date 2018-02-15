pragma solidity ^0.4.16;

contract Test {
  bool private flag=true;

  function set(int val) returns (bool){
    if (val % 100 == 0) {flag = false;}
  }
  function echidna_alwaystrue() returns (bool){
    return(true);
  }
  function echidna_sometimesfalse() returns (bool){
    return(flag);
  }
}
