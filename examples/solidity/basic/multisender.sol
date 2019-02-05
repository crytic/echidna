contract C {
  bool state1 = false;
  bool state2 = false;
  bool state3 = false;
  
  function s1(address x) {
    require(x == msg.sender);
    require(x == 0x1);
    state1 = true;
  }


  function s2(address x) {
    require(x == msg.sender);
    require(x == 0x2);
    state2 = true;
  }


  function s3(address x) {
    require(x == msg.sender);
    require(x == 0x3);
    state3 = true;
  }

  function echidna_all_sender() returns (bool) {
    return (!state1 || !state2 || !state3);
  }

}
