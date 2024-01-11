import "./immutable-3.sol";

contract C {
  D d;
  constructor() {
    d = new D(0);
  }
  function set(uint256 n) external {
    d = new D(n);
    d.set();
  }
  function echidna_test() public returns (bool) {
    return d.state();
  }
}
