contract ShouldNotRevert {	
  function f() public { }  	

   function echidna_should_not_revert() public returns (bool) {	
    bool[] memory includeMap = new bool[](1);	
    if (includeMap[0]) {	
      revert();	
    }	
    return true;	
  }	
}
