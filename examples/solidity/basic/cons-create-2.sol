contract A {}

contract C {
  bool state = true;
  bool state2 = true;
  A a;
  A b;

  constructor() public {
    a = new A();
  }

  function c() public {
    if (b == address(0))
      b = new A();
  }

  //function d() public returns (address) {
  //  return (new A());
  //}

  function f(address x) public {
    if (A(x) == a)
      state = false;
    if ((A(x) == b) && (b != address(0)))
      state2 = false;
  }

  function echidna_state() public returns (bool) {
    return state || state2;
  }
}
