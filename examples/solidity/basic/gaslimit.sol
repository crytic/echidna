contract C { 

  function echidna_gaslimit() public returns (bool) {
    return block.gaslimit > 0;
  }

}
