contract C {
  function echidna_expected_gasleft() public returns (bool) {
    uint256 left = gasleft();
    return left > 2000 && left < 5000;
  }
}
