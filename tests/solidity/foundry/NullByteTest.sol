// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract NullByteTest {
    // Simple function that takes bytes32 parameter
    function checkBytes(bytes32 data) public pure returns (bool) {
        return data != bytes32(0);
    }
}
