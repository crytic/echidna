contract D {
  uint256 public immutable n;
  bool public state = true;
  constructor(uint256 _n) {
    n = _n;
  }
  function set() external {
    if (n != 1) revert();
    state = false;
  }
}
