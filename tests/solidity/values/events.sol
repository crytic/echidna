contract EventAssert {
    bytes32 private hash;
    event Secret(uint256, string, bytes32);
    function reset(uint seed) public {
        require(hash == 0);
	bytes32 secret = keccak256(abi.encodePacked(seed));
	emit Secret(1, "abc", secret);
        hash = keccak256(abi.encodePacked(secret));
    }
    function check(bytes32 seed) public {
        if (keccak256(abi.encodePacked(seed)) == hash) {
            assert(false);
        }
    }
}
