pragma solidity ^0.4.16;

contract Test {
  event X(uint x);
  event Y(uint y);

  function logX(uint x) {
      emit X(x);
  }

  function logY(uint y) {
      emit Y(y);
  }

  function noevents(uint x, uint y) {
      return;
  }

  function echidna_alwaystrue() returns (bool){
      return(true);
  }

}
