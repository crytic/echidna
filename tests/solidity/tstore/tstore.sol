pragma solidity >=0.8.25;

contract Test {
  uint256 x;
  uint256 y;
  function A() public {
    assembly {
      if tload(0) { revert(0,0) }
      tstore(0, 1)
      if iszero(tload(0)) { revert(0,0) }
    }
    x = 5;
  }
  function B() public {
    if (x != 5) revert();
    assembly {
      if tload(0) { revert(0,0) }
    }
    y = 10;
  }
  function echidna_foo() public view returns (bool) {
    return y != 10;
  }
}
