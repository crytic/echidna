contract TEST {

  address [] a;
  bool lengthChecking = false;

  function push_1() public {
    a.push(address(1));
  }

  function turn_on_length_checking() public {
    lengthChecking = true;
  }

  function turn_off_length_checking() public {
    lengthChecking = false;
  }

  function test_long_5() public {
    if (a.length >= 5) {
      if (lengthChecking) {
	    assert(false);
      }
    }
  }
}
