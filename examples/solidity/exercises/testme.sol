pragma solidity ^0.4.16;

contract Canal {
  bool private first_gate_up=false;
  bool private second_gate_up=true;

  function lower(bool first_gate) returns (bool){
    if(first_gate) {
      if(second_gate_up) {
        first_gate_up = false;
        return(true);
      } else {
        return(false);
      }
    } else {
      if(first_gate_up) {
        second_gate_up = false;
        return true;
      } else {
        return(false);
      }
    }
  }

  function raise(bool first_gate) {
    if(first_gate) {
      first_gate_up = true;
    } else {
      second_gate_up = true;
    }
  }

  function echidna_bothdown() returns (bool) {
    return(first_gate_up || second_gate_up);
  }

  function echidna_can_lower_second() returns (bool) {
    bool a0 = lower(true);
    raise(true);
    return(a0);
  }
}
