pragma solidity 0.4.24;

import "AddressArrayUtils.sol";

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

  function echidna_find() public view returns (bool) {
    if (!everSet) {
      return true;
    }
    uint256 i;
    uint256 j;
    bool b;
    (i, b) = AddressArrayUtils.indexOf(addrs1, a);
    if (b) {
      if (addrs1[i] != a) {
	return false;
      }
      for (j = 0; j < i; j++) {
	if (addrs1[j] == a) {
	  return false;
	}
      }
      if (!AddressArrayUtils.contains(addrs1, a)) {
	return false;
      }
      (i, b) = AddressArrayUtils.indexOfFromEnd(addrs1, a);
      if (!b) {
	return false;
      }
      if (addrs1[i-1] != a) {
	return false;
      }      
      for (j = (addrs1.length-1); j >= i; j--) {
	if (addrs1[j] == a) {
	  return false;
	}
      }
    } else {
      for (j = 0; j < addrs1.length; j++) {
	if (addrs1[j] == a) {
	  return false;
	}
      }
      if (AddressArrayUtils.contains(addrs1, a)) {
	return false;
      }
      (i, b) = AddressArrayUtils.indexOfFromEnd(addrs1, a);
      if (b) {
	return false;
      }
    }
    return true;
  }

  function echidna_append() public view returns (bool) {
    if (!everSet) {
      return true;
    }    
    address [] memory addrs1PlusA = AddressArrayUtils.append(addrs1, a);
    uint256 i;
    uint256 j;
    bool b1;
    bool b2;
    if (AddressArrayUtils.isEqual(addrs1, addrs1PlusA)) {
      return false;
    }
    (i, b1) = AddressArrayUtils.indexOfFromEnd(addrs1PlusA, a);
    if (!b1) {
      return false;
    }
    if (i != (addrs1PlusA.length)) {
      return false;
    }
    if (addrs1PlusA[i-1] != a) {
      return false;
    }
    (j, b2) = AddressArrayUtils.indexOf(addrs1PlusA, a);
    if (!b2) {
      return false;
    }
    if (addrs1PlusA[j] != a) {
      return false;
    }
    if (AddressArrayUtils.contains(addrs1, a)) {
      if (j >= (i-1)) {
	return false;
      }
      if (!AddressArrayUtils.hasDuplicate(addrs1PlusA)) {
	return false;
      }
    } else {
      if (j != (i-1)) {
	return false;
      }
    }
    return true;
  }

  function echidna_extend() public view returns (bool) {
    if (!everSet) {
      return true;
    }    
    address [] memory addrs1PlusAddrs2 = AddressArrayUtils.extend(addrs1, addrs2);
    uint256 i;
    for (i = 0; i < addrs1.length; i++) {
      if (!AddressArrayUtils.contains(addrs1PlusAddrs2, addrs1[i])) {
	return false;
      }
    }
    for (i = 0; i < addrs2.length; i++) {
      if (!AddressArrayUtils.contains(addrs1PlusAddrs2, addrs2[i])) {
	return false;
      }
    }
    if (!AddressArrayUtils.contains(addrs1, a) && !AddressArrayUtils.contains(addrs2, a)) {
      if (AddressArrayUtils.contains(addrs1PlusAddrs2, a)) {
	return false;
      }
    }
    return true;
  }

  function echidna_sExtend() public returns (bool) {
    if (!everSet) {
      return true;
    }
    bool notInEither = !AddressArrayUtils.contains(addrs1, a) && !AddressArrayUtils.contains(addrs2, a);
    uint256 addrs1Length = addrs1.length;
    uint256 addrs2Length = addrs2.length;    
    AddressArrayUtils.sExtend(addrs1, addrs2);
    uint256 i;
    for (i = 0; i < addrs2.length; i++) {
      if (!AddressArrayUtils.contains(addrs1, addrs2[i])) {
	return false;
      }
    }
    if (addrs1.length != (addrs1Length + addrs2Length)) {
      return false;
    }
    if (notInEither && AddressArrayUtils.contains(addrs1, a)) {
      return false;
    }
    return true;
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

  function echidna_isEqual() public view returns (bool) {
    if (!everSet) {
      return true;
    }    
    uint256 length1 = addrs1.length;
    uint256 length2 = addrs2.length;
    bool areEqual = AddressArrayUtils.isEqual(addrs1, addrs2);
    if (length1 != length2) {
      return !areEqual;
    }
    for (uint i = 0; i < length1; i++) {
      if (addrs1[i] != addrs2[i]) {
	return !areEqual;
      }
    }
    return areEqual;
  }
  
  function echidna_sReverse() public returns (bool) {
    if (!everSet) {
      return true;
    }    
    uint256 length = addrs1.length;
    address a0;
    address a1;
    if (length > 0) {
      a0 = addrs1[0];
      a1 = addrs1[length-1];
    }
    AddressArrayUtils.sReverse(addrs1);
    if (addrs1.length != length) {
      return false;
    }
    if (length > 0) {
      if (a0 != addrs1[length-1]) {
	return false;
      }
      if (a1 != addrs1[0]) {
	return false;
      }
    }
    return true;
  }

  function ourEqual(address[] memory A, address[] memory B) internal pure returns (bool) {
    uint256 i;
    uint256 j;
    bool found;
    for (i = 0; i < A.length; i++) {
      found = false;
      for (j = 0; j < B.length; j++) {
	if (A[i] == B[j]) {
	  found = true;
	  break;
	}
      }
      if (!found) {
	return false;
      }
    }
    for (i = 0; i < B.length; i++) {
      found = false;
      for (j = 0; j < A.length; j++) {
	if (A[j] == B[i]) {
	  found = true;
	  break;
	}
      }
      if (!found) {
	return false;
      }
    }
    return true;    
  }

  function echidna_diff() public view returns (bool) {
    if (!everSet) {
      return true;
    }
    address [] memory empty = new address[](0);
    address [] memory diff1 = AddressArrayUtils.difference(addrs1, addrs2);
    address [] memory diff2 = AddressArrayUtils.difference(addrs2, addrs1);
    if (!ourEqual(addrs1, addrs2) == ourEqual(diff1, diff2)) {
      return false;
    }
    if (!ourEqual(AddressArrayUtils.difference(addrs1, addrs1), empty)) {
      return false;
    }
    return true;
  }

  function echidna_union() public view returns (bool) {
    if (!everSet) {
      return true;
    }
    address [] memory empty = new address[](0);
    address [] memory union1 = AddressArrayUtils.union(addrs1, addrs2);
    address [] memory union2 = AddressArrayUtils.union(addrs2, addrs1);
    address [] memory unionB1 = AddressArrayUtils.unionB(addrs1, addrs2);
    address [] memory unionB2 = AddressArrayUtils.unionB(addrs2, addrs1);    
    if (!ourEqual(union1, union2)) {
      return false;
    }
    if (!AddressArrayUtils.hasDuplicate(addrs1) && !AddressArrayUtils.hasDuplicate(addrs2)) {
      if (!ourEqual(unionB1, unionB2)) {
	return false;
      }
    }
    if (!ourEqual(AddressArrayUtils.union(addrs1, empty), addrs1)) {
      return false;
    }    
    return true;
  }

  function echidna_intersect() public view returns (bool) {
    if (!everSet) {
      return true;
    }
    address [] memory empty = new address[](0);
    address [] memory intersect1 = AddressArrayUtils.intersect(addrs1, addrs2);
    address [] memory intersect2 = AddressArrayUtils.intersect(addrs2, addrs1);
    if (!ourEqual(intersect1, intersect2)) {
      return false;
    }
    if (!ourEqual(AddressArrayUtils.intersect(addrs1, empty), empty)) {
      return false;
    }
    return true;
  }

  function echidna_remove() public view returns (bool) {
    if (!everSet) {
      return true;
    }    
    if (!AddressArrayUtils.contains(addrs1, a)) {
      return true;
    }
    uint256 account = 0;
    uint256 i;
    for (i = 0; i < addrs1.length; i++) {
      if (addrs1[i] == a) {
	account++;
      }
    }
    address [] memory removed = AddressArrayUtils.remove(addrs1, a);
    if (removed.length != (addrs1.length-1)) {
      return false;
    }
    uint256 accountNew = 0;
    for (i = 0; i < removed.length; i++) {
      if (removed[i] == a) {
	accountNew++;
      }
    }
    if (accountNew != (account-1)) {
      return false;
    }
    if (!AddressArrayUtils.hasDuplicate(addrs1)) {
      if (AddressArrayUtils.contains(removed, a)) {
	return false;
      }
    }
    return true;
  }

  function echidna_revert_remove() public view returns (bool) {
    if (!everSet) {
      revert();
    }
    if (AddressArrayUtils.contains(addrs1, a)) {
      revert();
    }
    AddressArrayUtils.remove(addrs1, a);
  }

  function echidna_pop() public view returns (bool) {
    if (!everSet) {
      return true;
    }    
    if (!AddressArrayUtils.contains(addrs1, a)) {
      return true;
    }
    uint256 aIndex;
    bool aFound;
    (aIndex, aFound) = AddressArrayUtils.indexOf(addrs1, a);
    uint256 account = 0;
    uint256 i;
    for (i = 0; i < addrs1.length; i++) {
      if (addrs1[i] == a) {
	account++;
      }
    }
    address [] memory removed;
    address apop;
    (removed, apop) = AddressArrayUtils.pop(addrs1, aIndex);
    if (apop != a) {
      return false;
    }
    if (removed.length != (addrs1.length-1)) {
      return false;
    }
    uint256 accountNew = 0;
    for (i = 0; i < removed.length; i++) {
      if (removed[i] == a) {
	accountNew++;
      }
    }
    if (accountNew != (account-1)) {
      return false;
    }
    if (!AddressArrayUtils.hasDuplicate(addrs1)) {
      if (AddressArrayUtils.contains(removed, a)) {
	return false;
      }
    }
    return true;
  }

  function echidna_sRemoveCheap() public returns (bool) {
    if (!everSet) {
      return true;
    }    
    if (!AddressArrayUtils.contains(addrs1, a)) {
      return true;
    }
    uint256 account = 0;
    uint256 i;
    for (i = 0; i < addrs1.length; i++) {
      if (addrs1[i] == a) {
	account++;
      }
    }
    uint256 oldLength = addrs1.length;
    bool anyDuplicates = AddressArrayUtils.hasDuplicate(addrs1);
    AddressArrayUtils.sRemoveCheap(addrs1, a);
    if (addrs1.length != (oldLength-1)) {
      return false;
    }
    uint256 accountNew = 0;
    for (i = 0; i < addrs1.length; i++) {
      if (addrs1[i] == a) {
	accountNew++;
      }
    }
    if (accountNew != (account-1)) {
      return false;
    }
    if (!anyDuplicates) {
      if (AddressArrayUtils.contains(addrs1, a)) {
	return false;
      }
    }
    return true;
  }

  function echidna_revert_sRemoveCheap() public returns (bool) {
    if (!everSet) {
      revert();
    }
    if (AddressArrayUtils.contains(addrs1, a)) {
      revert();
    }
    AddressArrayUtils.sRemoveCheap(addrs1, a);
  }

  function echidna_sPop() public returns (bool) {
    if (!everSet) {
      return true;
    }    
    if (!AddressArrayUtils.contains(addrs1, a)) {
      return true;
    }
    uint256 aIndex;
    bool aFound;
    (aIndex, aFound) = AddressArrayUtils.indexOf(addrs1, a);
    uint256 account = 0;
    uint256 i;
    for (i = 0; i < addrs1.length; i++) {
      if (addrs1[i] == a) {
	account++;
      }
    }    
    uint256 oldLength = addrs1.length;
    bool anyDuplicates = AddressArrayUtils.hasDuplicate(addrs1);
    address apop = AddressArrayUtils.sPop(addrs1, aIndex);
    if (apop != a) {
      return false;
    }
    if (addrs1.length != (oldLength-1)) {
      return false;
    }
    uint256 accountNew = 0;
    for (i = 0; i < addrs1.length; i++) {
      if (addrs1[i] == a) {
	accountNew++;
      }
    }
    if (accountNew != (account-1)) {
      return false;
    }
    if (!anyDuplicates) {
      if (AddressArrayUtils.contains(addrs1, a)) {
	return false;
      }
    }
    return true;
  }

  function echidna_revert_sPop() public returns (bool) {
    if (!everSet) {
      revert();
    }
    uint256 index = addrs1.length;
    AddressArrayUtils.sPop(addrs1, index);
  }

  function echidna_sPopCheap() public returns (bool) {
    if (!everSet) {
      return true;
    }    
    if (!AddressArrayUtils.contains(addrs1, a)) {
      return true;
    }
    uint256 aIndex;
    bool aFound;
    (aIndex, aFound) = AddressArrayUtils.indexOf(addrs1, a);
    uint256 account = 0;
    uint256 i;
    for (i = 0; i < addrs1.length; i++) {
      if (addrs1[i] == a) {
	account++;
      }
    }    
    uint256 oldLength = addrs1.length;
    bool anyDuplicates = AddressArrayUtils.hasDuplicate(addrs1);
    address apop = AddressArrayUtils.sPopCheap(addrs1, aIndex);
    if (apop != a) {
      return false;
    }
    if (addrs1.length != (oldLength-1)) {
      return false;
    }
    uint256 accountNew = 0;
    for (i = 0; i < addrs1.length; i++) {
      if (addrs1[i] == a) {
	accountNew++;
      }
    }
    if (accountNew != (account-1)) {
      return false;
    }
    if (!anyDuplicates) {
      if (AddressArrayUtils.contains(addrs1, a)) {
	return false;
      }
    }
    return true;
  }

  function echidna_revert_sPopCheap() public returns (bool) {
    if (!everSet) {
      revert();
    }
    uint256 index = addrs1.length;
    AddressArrayUtils.sPopCheap(addrs1, index);
  }
  
  function echidna_argGet() public view returns (bool) {
    if (!everSet) {
      return true;
    }
    if (addrs1.length < 1) {
      return true;
    }
    bool found;
    uint256 index;
    uint256 i;
    uint256[] memory indexArray = new uint256[](addrs2.length);
    for (i = 0; i < indexArray.length; i++) {
      (index, found) = AddressArrayUtils.indexOf(addrs1, addrs2[i]);
      if (found) {
	indexArray[i] = index;
      } else {
	indexArray[i] = 0;
      }
    }
    address[] memory argGetResult = AddressArrayUtils.argGet(addrs1, indexArray);
    for (i = 0; i < argGetResult.length; i++) {
      if (argGetResult[i] != addrs1[indexArray[i]]) {
	return false;
      }
    }
    return true;
  }
  
}
