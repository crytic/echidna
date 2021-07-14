contract C { 
  bytes e = "abcdef123";
  bool state;

  function f(bytes memory bs) public {
    if (bs[0] == e[0] && bs[1] == e[1] && bs[2] == e[2] && bs.length > 16)
        state = true;
  }

  function echidna_mutated() public returns (bool) {
    return !state;
  }

}
