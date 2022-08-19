contract ConstantsAddress {
  bool found = false;

  function find(address s) public {
    if (s == msg.sender) {found = true;}
  }

  function echidna_found_sender() public view returns (bool) {
    return(!found);
  }
}

