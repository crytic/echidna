// Exercises per-line hit counts: a loop whose body runs many times per call,
// and a function whose every execution is inside a failing transaction.
contract HitCounts {
  uint public total;

  function loop(uint8 n) public {
    for (uint i = 0; i < n; i++) {
      total += i;
    }
  }

  function alwaysReverts(uint x) public {
    total += x;
    revert("no");
  }

  function echidna_true() public returns (bool) {
    return true;
  }
}
