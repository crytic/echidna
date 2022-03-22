contract Test {
  TestAssert ta;
  constructor() public {
    ta = new TestAssert();
  }

  function f() public {}
}

contract TestAssert { 
  event AssertionFailed(string message);
  function fail(uint val) public {
    if(val > 128)
      emit AssertionFailed("error");
  }
  function g() public { }
}
