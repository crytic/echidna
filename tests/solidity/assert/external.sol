contract Test {
  TestAssert ta;
  constructor() public {
    ta = new TestAssert();
  }

  function assert_external(uint val) public {
    ta.fail(val);
  }

  function external_call() public returns (bool){
    ta.f();
  }

}

contract TestAssert { 
  event AssertionFailed(string message);
  function fail(uint val) public {
    if(val > 128)
      emit AssertionFailed("error");
  }
  function f() public { }
}
