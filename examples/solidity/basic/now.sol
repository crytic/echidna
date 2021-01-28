contract C {
  bool state;
  uint time;

  function set() public {
    time = block.timestamp;
  }

  function guess(uint x) public {
    if (x <= time + 1 weeks && x >= time - 1 weeks ) 
      state = true;
  }

  function echidna_now() public returns(bool) {
    return (!state);
  }

} 
