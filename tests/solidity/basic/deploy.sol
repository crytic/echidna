contract A {
  uint256 public x = 128;
}

contract Test {
  function test(uint256 y) public {
    assert(A(address(0x42)).x() == y);
  }
}
