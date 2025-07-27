contract EventAssert {
    uint private secret;
    event Secret(uint result);
    constructor() {
	reset();
    }
    
    function reset() public {
        secret = uint(keccak256(abi.encodePacked(block.timestamp)));
        emit Secret(secret);
    }
    
    function check(uint x) public {
        if (x == secret) {
            assert(false);
        }
    }
}
