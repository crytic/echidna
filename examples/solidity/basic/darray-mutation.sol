contract C { 
  bytes example = "abcdef123";
  bool state;

  function f(bytes memory bs) public {
    if (bs[0] == "a" && bs[1] == "b" && bs[2] == "c" && bs.length > 16)
        state = true;
  }

  function echidna_mutated() public returns (bool) {
    return !state;
  }

}
