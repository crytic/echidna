pragma experimental ABIEncoderV2;

contract C {
  struct T {
    bytes bs;
  }
  struct S {
    T t;
  }

  bool state = true;

  function f(S memory o1, S memory o2) public {
    state = false;
  }

  function echidna_test() public returns (bool) {
    return state;
  }
}
