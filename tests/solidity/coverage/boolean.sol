contract C {
  uint state = 0;

  function f(bool sel, bool b) payable public {
    state = 1;
    if (sel)
      sel = false;
    else 
      sel = true;
    require(b);
  }

  function echidna_true() public returns (bool) { 
    return true;
  }

}
