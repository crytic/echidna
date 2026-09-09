// A and B compile to identical runtime bytecode (modulo metadata) but different
// creation bytecode: their constructors are distinct code units that share one
// owning runtime codehash. Main deploys both from its own constructor.
contract A {
  uint public x;
  constructor() { x = 1; }
  function f() public { x += 1; }
}

contract B {
  uint public x;
  constructor() { x = 2; }
  function f() public { x += 1; }
}

contract Main {
  A a;
  B b;
  constructor() {
    a = new A();
    b = new B();
  }
  function poke() public {
    a.f();
    b.f();
  }
  function echidna_true() public returns (bool) {
    return true;
  }
}
