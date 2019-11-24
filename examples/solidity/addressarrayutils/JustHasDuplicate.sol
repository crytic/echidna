pragma solidity 0.4.24;

import "AddressArrayUtils_withHasDuplicateBug.sol";

contract TEST {

  address [] addrs1;
  address [] addrs2;
  address a;

  bool everSet = false;
  
  function push_it_1() public {
    if (everSet) {
      addrs1.push(a);
    }
  }

  function push_it_2() public {
    if (everSet) {
      addrs2.push(a);
    }
  }
    
  function push_it_both() public {
    if (everSet) {
      addrs1.push(a);
      addrs2.push(1);      
    }
  }

  function set_addr(address newa) public {
    everSet = true;
    a = newa;
  }

  function echidna_hasDuplicate() public view returns (bool) {
    if (!everSet) {
      return true;
    }
    bool hasDup = false;
    uint i1;
    uint i2;
    bool b;
    for (uint i = 0; i < addrs1.length; i++) {
      (i1, b) = AddressArrayUtils.indexOf(addrs1, addrs1[i]);
      (i2, b) = AddressArrayUtils.indexOfFromEnd(addrs1, addrs1[i]);
      if (i1 != (i2-1)) {
	hasDup = true;
      }
    }
    return hasDup == AddressArrayUtils.hasDuplicate(addrs1);
  }
}
