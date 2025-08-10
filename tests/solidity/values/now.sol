contract C {
  bool state;
  uint time;

  function set() public {
    if (time == 0)
      time = block.timestamp;
  }

  function guess(uint x) public {
    if (time > 0 && x <= time + 4 weeks && x >= time - 4 weeks )
      state = true;
  }

  function echidna_now() public returns(bool) {
    return (!state);
  }

} 
