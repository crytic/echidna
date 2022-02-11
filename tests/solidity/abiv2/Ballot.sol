pragma experimental ABIEncoderV2;

contract Ballot {
  struct Voter { // Struct
    uint weight;
    bool voted;
    address delegate;
    uint vote;
  }

  bool cond = true;

  function add_voter(Voter memory voter) public returns (bool) {
    if (voter.voted == true) {
      cond = false;
    }
    return(true);
  }

  function echidna_test() public returns (bool) {
    return(cond);
  }
}
