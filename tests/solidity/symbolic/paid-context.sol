// SPDX-License-Identifier: MIT
pragma solidity >=0.6.9;

contract PaidContext {
    uint256 private phase;
    uint256 private startTime = block.timestamp;
    uint256 private startBlock = block.number;

    function deposit() public payable {
        require(phase == 0 && msg.value == 3);
        require(block.timestamp == startTime + 2 && block.number == startBlock + 1);
        require(address(this).balance == 3);
        phase = 1;
    }

    function finish() public payable {
        require(phase == 1 && msg.value == 6);
        require(block.timestamp == startTime + 4 && block.number == startBlock + 2);
        require(address(this).balance == 9);
        phase = 2;
    }

    function checkFinished(uint256 key) public view {
        require(key == 44);
        assert(phase != 2);
    }

    function single(uint256 key) public payable {
        require(key == 33 && msg.value == 5);
        require(block.timestamp == startTime + 2 && block.number == startBlock + 1);
        require(address(this).balance == 5);
        assert(false);
    }

    function echidna_balance() public view returns (bool) {
        return address(this).balance < 9;
    }
}
