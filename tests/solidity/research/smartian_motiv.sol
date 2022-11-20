// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract C {
  address owner;
  uint private stateA = 0;
  uint private stateB = 0;
  uint constant CONST = 32;
  bool bug = false;

  constructor() { // Constructor
    owner = msg.sender;
  }

  function f(uint x) public {
    if (msg.sender == owner) { 
      stateA = x; 
    }
  }

  function g(uint y) public {
    if (stateA % CONST == 1) {
      stateB = y - 10;
    }
  }

  function h() public {
    if (stateB == 62) { 
      bug = true; 
    }
  }

  function echidna_bug() public returns (bool) {
    return !bug;
  }
}
