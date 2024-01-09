contract D {
  uint256 public immutable n;
  bool public state = true;
  constructor(uint256 _n) {
    n = _n;
  }
  function set(uint256 m) external {
    if (n+1 != 101) revert();
    if (m+1 != 104) revert();
    state = false;
  }
}
