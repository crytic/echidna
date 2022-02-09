contract C {
  uint sz;

  constructor() public {
    uint x;
    address t = address(this);
    assembly { x := extcodesize(t) }
    sz = x;
  }

  function echidna_sz_zero() public returns (bool) {
    return sz == 0;
  }
}
