contract C {
  uint blockA;
  uint timeA;
  uint blockB;
  uint timeB;

  constructor() public {
    blockA = block.number;
    timeA = block.timestamp;
    blockB = block.number;
    timeB = block.timestamp;
  }

  function f() public {
    blockA = block.number;
    timeA = block.timestamp;
  }

  function g() public {
    blockB = block.number;
    timeB = block.timestamp;
  }

  function echidna_block_number() public returns (bool) {
    return blockA == blockB;
  }

  function echidna_timestamp() public returns (bool) {
    return timeA == timeB;
  }
}
