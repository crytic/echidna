contract F {
  bool fail;
  function init(bool x) public {
    fail = x;
  }

  function is_f() public returns (bool) {
    return true;
  }

  function f() public returns (bool) {
    return fail;
  }
}

contract Test {
  bool firstRun = true;
  bool tested = false;

  function spawn() public {
    (new F()).init(firstRun);
    firstRun = false;
  }

  function verify_first(F f) public {
    require(f.is_f());
    assert(f.f() == true);
    // writes state to make it non-pure and more likely for echidna to call
    tested = true;
  }

  function verify_later(F f) public {
    require(f.is_f());
    assert(f.f() == false);
    // writes state to make it non-pure and more likely for echidna to call
    tested = true;
  }
}
