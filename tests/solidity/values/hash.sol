contract C {
    bool internal b;
    mapping(bytes32=>uint256) internal state;
    
    function f() public {
      require(!b);
      state[keccak256(abi.encode(block.number))] = 1;
      b = true;
    }

    function g(bytes32 x) public {
      assert(state[x] == 0);
      b = true;
    }
}
