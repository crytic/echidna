contract A {
  function c() public {
  }

  B private b;
  constructor() public {
    b = new B();
  }
}
contract B {
  function c() public {
    assert(false);
  }
}
