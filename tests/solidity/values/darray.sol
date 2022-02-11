contract C {

  address x = address(0x123456);
  bool state;

  function add_darray(address[] memory xs) public {
    for (uint i; i < xs.length; i++)
      if (xs[i] == x)
        state = true;
  }

  function echidna_darray() public returns (bool) {
    return !state;
  }

}
