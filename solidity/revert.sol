pragma solidity ^0.4.16;

contract C {

  int private state = 0;
  function f(int x) public {
    state = x;
  }

  function g() public returns (bool) {

    if (state < 0)
      require(false);

  }
}
