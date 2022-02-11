contract Test {

  function direct_assert(uint val) public {
    assert(val <= 128);
  }

  function internal_func(uint val) internal {
    assert(val <= 128);
  }

  function f() public {}

  function internal_assert(uint val) public {
    internal_func(val);
  }

}
