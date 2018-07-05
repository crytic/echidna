pragma solidity ^0.4.16;

contract Test {
  bool private flag0=true;
  bool private flag1=true;

  function set0(int val) returns (bool){
    if (val % 10 == 0) {flag0 = false;}
  }
  function set1(int val) returns (bool){
    if (val % 10 == 0 && flag0) {flag1 = false;}
  }
  function deepstate_alwaystrue() returns (bool){
    return(true);
  }
  function deepstate_sometimesfalse() returns (bool){
    return(flag0 || flag1);
  }
}
