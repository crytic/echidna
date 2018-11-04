pragma solidity ^0.4.22;

contract Test {
  uint private val;

  constructor (uint _val) public {
    require(_val > 0);
    val = _val;
  }

  function inc() public {
    val++;
  }

  function echidna_check() public view returns (bool){
    return val > 0;
  }
}
