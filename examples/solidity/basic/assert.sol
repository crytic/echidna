contract Test {
  event AssertionFailed(string message);
  function set0(int val) public returns (bool){
    assert(val % 100 != 0);
  }

  function f(uint val) public {
    require(msg.sender == address(this));
    
    if(val > 125)
      emit AssertionFailed("error");
  }

  function g(uint val) public {
  }

  function set1(uint val) public returns (bool){
    this.f(val);
  }
}
