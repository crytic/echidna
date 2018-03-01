pragma solidity ^0.4.16;

contract Turnstile {
  bool private locked = true; // initial state is locked

  function coin() {
    locked = true;
  }

  function push() returns (bool) {
    if (locked) {
      return(false);
    } else {
      locked = true;
      return(true);
    }
  }
}
