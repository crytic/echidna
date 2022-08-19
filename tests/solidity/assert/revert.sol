contract Test {
  event AssertionFailed(string message);
  function assert_revert(uint val) public {
    if(val > 128) {
      emit AssertionFailed("error");
      revert();
    } 
  }
  function assert_unreachable() public {
    revert();
    // unreachable
    emit AssertionFailed("error");
  }
  
}
