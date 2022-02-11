contract Abstract {
  constructor() public { require(false);}
  function f() pure public returns (int) {return 0;}
  function echidna_f() pure public returns (bool) {return false;}
}
