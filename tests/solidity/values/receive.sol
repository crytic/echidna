pragma solidity ^0.6.0;

contract Receive {
  bool found_fallback = false;
  bool found_receive = false;

  function echidna_found_fallback() public view returns (bool) {
    return(!found_fallback);
  }

  function echidna_found_receive() public view returns (bool) {
    return(!found_receive);
  }

  fallback() external payable {
    found_fallback = true;
  } 

  receive() external payable {
    found_receive = true;
  } 
}
