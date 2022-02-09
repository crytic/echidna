pragma solidity >=0.8.0;

contract Test {

  function direct_assert(int val) public {
    assert(val % 100 != 0);
  }

  function f(uint val) public {}
  function g() public { revert(); }

}
