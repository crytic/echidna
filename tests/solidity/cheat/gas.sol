pragma experimental ABIEncoderV2;

interface Hevm {
  function assume(bool) external;
}

contract C {
  address public calledContract;

  constructor() public {
    calledContract = 0x7109709ECfa91a80626fF3989D68f67F5b1DD12D;
  }

  function foo() public returns (uint256) {
        assembly {
            mstore(0x80, 0x4c63e562) 
            mstore(0xa0, 1)

            let addr := sload(0 /* calledContract.slot */)
            let beforegas := gas()
            let success := callcode(gas(), addr, 0, 0x9c, 0x24, 0, 0)
            let aftergas := gas()
            mstore(0x80, sub(beforegas, aftergas))
            return(0x80, 0x20)
        }
  }
}

contract TestCheatGas {
  uint256 spent = 123456;
  C foo;

  constructor() public {
    foo = new C();
  }

  function bar() public {
    spent = foo.foo();
  }

  function echidna_gas_zero() public returns (bool){
    // 0x14 as measured from opcodes, but let's leave some leeway in case solc changes
    // GAS PUSH1 0x0 DUP1 PUSH1 0x24 PUSH1 0x9C PUSH1 0x0 DUP7 GAS CALLCODE GAS
    return spent > 0x20;
  }
}
