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
    if (b == A(address(0x0)))
      b = new A();
  }

  function f(address x) public {
    if (A(x) == a)
      state = false;
    if ((A(x) == b) && (b != A(address(0x0))))
      state2 = false;
  }

  function echidna_state() public returns (bool) {
    return state || state2;
  }
}
