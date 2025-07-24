// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract C {
  address owner;
  int private stateA = 0;
  int private stateB = 0;
  int constant CONST = 30007;

  function f(int x) public {
    if (x % CONST == 4565) { 
      stateA = x; 
    }
  }

  function g(int y) public {
    if (stateA > 0) {
      stateB = stateA ^ 4555666644 + y;
      assert(stateB != 62);
    }
  }
}
