contract C {

  bytes32 public immutable x = keccak256("");
  bool private state = true;
  function f() public { state = false; }
  function echidna_test() public returns (bool) { return state; }

}
