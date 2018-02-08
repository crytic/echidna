pragma solidity ^0.4.16;

contract Test {
  uint private counter=2**250;

  function inc(uint val) returns (uint){
    uint tmp = counter;
    counter += val;
    if (tmp > counter) {selfdestruct(0);}
    else {return (counter - tmp);}
  }
  function boom() returns (bool){
    return(true);
  }
}
