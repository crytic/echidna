contract C {

  function f(bool sel, bool b) public {
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
