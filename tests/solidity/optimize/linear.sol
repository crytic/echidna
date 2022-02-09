contract Test {
  int256 input;


  function set(int256 _input) public {
    input = _input;
  }

  function echidna_opt_linear() public returns (int256) {
    if (input > -4242)
      return -input;
    else
      return 0;
  }

}
