contract Arithmetic {
  function divMulBound(uint256 x, uint256 y) public pure {
    require(y != 0);
    uint256 product;
    assembly { product := mul(div(x, y), y) }
    assert(product <= x);
  }

  // Keep modulo refinement tractable for both Z3 and Bitwuzla.
  function modBound(uint8 x, uint8 y) public pure {
    require(y != 0);
    assert(x % y < y);
  }

  function mulRefinement(uint256 x, uint256 y) public pure {
    require(x > 1 && x < 4 && y > 1 && y < 4);
    uint256 product;
    assembly { product := mul(x, y) }
    // Both operands stay symbolic. Exact refinement must reject product == 5.
    assert(product != 5);
  }

  function divCounterexample(uint256 x, uint256 y) public pure {
    require(y != 0);
    // False when x < y; the refined model must reproduce this failure.
    assert(x / y != 0);
  }

  function mulCounterexample(uint256 x, uint256 y) public pure {
    require(x > 1 && x < 4 && y > 1 && y < 4);
    uint256 product;
    assembly { product := mul(x, y) }
    // False for (2, 3) and (3, 2).
    assert(product != 6);
  }
}
