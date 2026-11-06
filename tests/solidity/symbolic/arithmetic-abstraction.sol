contract ArithmeticAbstraction {
  // Bounds prevent overflow, so multiplication preserves the ordering.
  // Inspired by hevm's mul-monotone arithmetic-abstraction test.
  // With solc 0.8.25, native arithmetic exhausts a 30s SMT budget under
  // both Z3 4.15.4 and Bitwuzla 0.8.2; arithmetic abstraction proves it.
  function mulMonotone(uint256 x, uint256 y, uint256 k) public pure {
    require(x < (1 << 128) && y < (1 << 128) && k < (1 << 128));
    require(x <= y);
    uint256 xk;
    uint256 yk;
    assembly {
      xk := mul(x, k)
      yk := mul(y, k)
    }
    assert(xk <= yk);
  }
}
