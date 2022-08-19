contract C { 
  bytes32 example = "abcdef123";
  bool state;

  function f(bytes32 bs) public {
    if (bs[0] == "a" && bs[1] == "b" && bs[2] == "c" && bs != example)
        state = true;
  }

  function echidna_mutated() public returns (bool) {
    return !state;
  }

}
