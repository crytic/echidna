// SPDX-License-Identifier: MIT
pragma solidity >=0.6.9;

contract StatefulChecks {
    bool private ready;
    bool private flag;
    uint256 private x;
    uint256 private timestamp;
    uint256 private number;
    address private writer;

    function prepare() public { ready = true; }
    function enable() public { flag = true; }

    function set(uint256 value) public {
        require(ready && value < 100);
        x = value;
        timestamp = block.timestamp;
        number = block.number;
        writer = msg.sender;
    }

    function echidna_value() public view returns (bool) { return x != 42; }
    function echidna_noarg() public view returns (bool) { return !flag; }

    function echidna_reverts() public view returns (bool) {
        require(x != 43);
        return true;
    }

    function echidna_revert_expected() public view returns (bool) {
        require(x == 44);
        return true;
    }

    function echidna_context() public view returns (bool) {
        return timestamp == block.timestamp && number == block.number
            && msg.sender == address(0xcafe) && tx.origin == address(0xcafe);
    }

    function echidna_loop() public view returns (bool) {
        if (x == 45) { while (true) {} }
        return true;
    }

    function check_sender() public view {
        assert(x != 46 || msg.sender == writer);
    }
}
