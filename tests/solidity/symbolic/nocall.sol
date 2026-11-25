// SPDX-License-Identifier: MIT
pragma solidity >=0.6.9;

contract NoCallContext {
    uint256 private startTime = block.timestamp;
    uint256 private startBlock = block.number;
    bool private reached;

    function capture() public {
        require(block.timestamp == startTime + 2 && block.number == startBlock + 2);
        reached = true;
    }

    function assertAfterWait() public view {
        require(block.timestamp == startTime + 2 && block.number == startBlock + 2);
        assert(false);
    }

    function blocked() public pure {
        revert();
    }

    function echidna_not_reached() public view returns (bool) {
        return !reached;
    }

    function echidna_before_deadline() public view returns (bool) {
        return block.timestamp < startTime + 2 || block.number < startBlock + 2;
    }
}
