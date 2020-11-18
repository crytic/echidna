contract Time {
  uint start;
  uint marked;

  constructor() public {
    start  = now;
    marked = now;
  }

  function mark() public {
    marked = now;
  }

  function echidna_timepassed() public returns (bool) {
    return(start == marked);
  }

  function echidna_moretimepassed() public returns (bool) {
    return(now < start + 10 weeks );
  }

}
