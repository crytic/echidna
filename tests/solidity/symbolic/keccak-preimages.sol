contract KeccakPreimages {
  bytes32 private digest;

  constructor() public {
    remember(0x123456789abcdef0123456789abcdef);
  }

  function remember(uint256 secret) public {
    digest = keccak256(abi.encode(secret));
  }

  function checkHash(uint256 candidate) public view {
    assert(keccak256(abi.encode(candidate)) != digest);
  }
}
